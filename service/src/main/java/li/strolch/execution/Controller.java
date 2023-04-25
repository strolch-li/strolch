package li.strolch.execution;

import static java.util.Collections.synchronizedMap;
import static li.strolch.execution.EventBasedExecutionHandler.PROP_LOCK_RETRIES;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import li.strolch.agent.api.ObserverEvent;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchLockException;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.execution.command.*;
import li.strolch.execution.policy.ExecutionPolicy;
import li.strolch.model.Locator;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.Tags;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.privilege.PrivilegedRunnable;
import li.strolch.runtime.privilege.PrivilegedRunnableWithResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Controller {

	protected static final Logger logger = LoggerFactory.getLogger(Controller.class);

	protected final int lockRetries;
	protected final String realm;
	protected final StrolchAgent agent;
	protected final ExecutionHandler executionHandler;

	protected final String activityType;
	protected final String activityId;
	protected final Locator locator;

	private final Map<Locator, ExecutionPolicy> inExecution;

	protected Activity activity;

	public Controller(String realm, ExecutionHandler executionHandler, Activity activity) {
		this.realm = realm;
		this.agent = executionHandler.getAgent();
		this.executionHandler = executionHandler;
		this.locator = activity.getLocator();
		this.activityType = activity.getType();
		this.activityId = activity.getId();
		this.activity = activity;
		this.inExecution = synchronizedMap(new HashMap<>());
		this.lockRetries = executionHandler.getConfiguration().getInt(PROP_LOCK_RETRIES, 2);
	}

	public String getRealm() {
		return this.realm;
	}

	public ExecutionHandler getExecutionHandler() {
		return this.executionHandler;
	}

	public StrolchAgent getAgent() {
		return this.agent;
	}

	public boolean isStopped(Locator locator) {
		ExecutionPolicy executionPolicy = this.inExecution.get(locator);
		return executionPolicy == null || executionPolicy.isStopped();
	}

	public Locator getLocator() {
		return this.locator;
	}

	public Activity getActivity() {
		return this.activity;
	}

	public State getState() {
		return this.activity.getState();
	}

	public Set<Locator> getInExecutionActionLocators() {
		return this.inExecution.keySet();
	}

	public ExecutionPolicy getExecutionPolicy(Locator actionLoc) {
		return this.inExecution.get(actionLoc);
	}

	public ExecutionPolicy refreshExecutionPolicy(StrolchTransaction tx, Action action) {
		ExecutionPolicy executionPolicy = this.inExecution.computeIfAbsent(action.getLocator(), e -> {
			Resource resource = tx.readLock(tx.getResourceFor(action, true));
			return tx.getPolicy(resource, ExecutionPolicy.class);
		});

		// always update the TX and controller
		executionPolicy.refreshController(tx, this);
		return executionPolicy;
	}

	public void removeExecutionPolicy(Action action) {
		this.inExecution.remove(action.getLocator());
	}

	protected StrolchTransaction openTx(Certificate cert) {
		return this.executionHandler.openTx(this.realm, cert, getClass(), false);
	}

	protected void runAsAgent(PrivilegedRunnable runnable) throws Exception {
		this.agent.runAsAgent(runnable);
	}

	protected <T> T runAsAgentWithResult(PrivilegedRunnableWithResult<T> runnable) throws Exception {
		return this.agent.runAsAgentWithResult(runnable);
	}

	@SuppressWarnings("BooleanMethodIsAlwaysInverted")
	private boolean refreshActivity(StrolchTransaction tx) {
		Activity activity = tx.getActivityBy(this.activityType, this.activityId, false);
		if (activity == null) {
			logger.error("Element " + this.locator + " does not exist anymore. Removing from execution");
			this.executionHandler.removeFromExecution(this);
			return false;
		}

		this.activity = activity;
		return true;
	}

	/**
	 * Stops the execution of all actions of this controller
	 */
	public void stopExecutions() {
		synchronized (this.inExecution) {
			this.inExecution.values().forEach(ExecutionPolicy::stop);
		}
	}

	/**
	 * Executes {@link Action Actions} for {@link Activity} of {@link Controller#getLocator()}. Keeps executing till no
	 * {@link Action} was set to {@link State#EXECUTED}
	 */
	public boolean execute() throws Exception {
		return runAsAgentWithResult(ctx -> {
			try (StrolchTransaction tx = openTx(ctx.getCertificate())) {
				lockWithRetries(tx);
				if (!refreshActivity(tx))
					return false;

				boolean trigger = internalExecute(tx);

				if (tx.needsCommit())
					tx.commitOnClose();

				return trigger;
			}
		});
	}

	/**
	 * Executes the activity in the given TX. Keeps executing till no {@link Action} was set to {@link State#EXECUTED}
	 *
	 * @param tx
	 * 		the TX
	 */
	public void execute(StrolchTransaction tx) {
		lockWithRetries(tx);
		if (!refreshActivity(tx))
			return;

		boolean trigger = internalExecute(tx);

		// we trigger execution for the same activity if the controller says it is needed
		if (trigger) {
			logger.info("Triggering additional execution of controller " + this + " after execution.");
			triggerExecute(tx);
		}
	}

	/**
	 * Executes the activity in the given TX by calling the {@link PlanAndExecuteActivityCommand}
	 *
	 * @param tx
	 * 		the TX
	 *
	 * @return true if execute should be called again, i.e. the
	 * {@link PlanAndExecuteActivityCommand#needsRetriggerOfExecution()} returns true and the activity isn't complete
	 * yet
	 */
	protected boolean internalExecute(StrolchTransaction tx) {
		if (this.activity.getState().isExecuted()) {
			this.executionHandler.removeFromExecution(this);
			logger.info("Archiving executed activity " + this.locator + " with state " + this.activity.getState());
			this.executionHandler.archiveActivity(this.realm, this.activity.getLocator());
			return false;
		}

		ExecutionHandlerState state = this.executionHandler.getExecutionState(this.realm);
		if (state == ExecutionHandlerState.Paused) {
			logger.warn("Ignoring trigger for paused realm " + this.realm);
			return false;
		}

		PlanAndExecuteActivityCommand command = new PlanAndExecuteActivityCommand(tx);
		command.setController(this);
		command.validate();
		command.doCommand();

		updateObservers();

		return command.needsRetriggerOfExecution();
	}

	/**
	 * Completes the execution of the given {@link Action} with the given {@link Locator}, executing next
	 * {@link Action Actions} if possible
	 *
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action} to set to executed
	 */
	public void toExecuted(Locator actionLoc) throws Exception {
		runAsAgent(ctx -> {
			try (StrolchTransaction tx = openTx(ctx.getCertificate())) {
				if (invalidActionContext(tx, actionLoc))
					return;

				Action action = this.activity.getElementByLocator(actionLoc);
				setToExecuted(tx, action);

				// now try and execute the next action(s)
				triggerExecute(tx);

				if (tx.needsCommit())
					tx.commitOnClose();
			}
		});
	}

	/**
	 * Completes the execution of the given {@link Action}. No further processing is done.
	 *
	 * @param tx
	 * 		the TX
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action} to set to executed
	 */
	public void toExecuted(StrolchTransaction tx, Locator actionLoc) {
		if (invalidActionContext(tx, actionLoc))
			return;

		Action action = this.activity.getElementByLocator(actionLoc);
		setToExecuted(tx, action);
	}

	private boolean invalidActionContext(StrolchTransaction tx, Locator actionLoc) {
		lockWithRetries(tx);
		if (!this.inExecution.containsKey(actionLoc))
			throw new IllegalStateException(actionLoc + " is not in execution!");
		return !refreshActivity(tx);
	}

	/**
	 * <p>Simply calls the {@link SetActionToExecutedCommand} and then updates the observers</p>
	 *
	 * <p><b>Note:</b> Usually you will want to call {@link #toExecuted(Locator)} or
	 * {@link #toExecuted(StrolchTransaction, Locator)}. This method expects the associated {@link Activity} to already
	 * be locked, and validated that this action is in execution</p>
	 *
	 * @param tx
	 * 		the TX
	 * @param action
	 * 		the Action to set to executed
	 */
	public void setToExecuted(StrolchTransaction tx, Action action) {

		// set this action to executed
		SetActionToExecutedCommand command = new SetActionToExecutedCommand(tx);
		command.setExecutionPolicy(refreshExecutionPolicy(tx, action));
		command.setAction(action);
		command.validate();
		command.doCommand();

		updateObservers();
	}

	/**
	 * <p>Keeps triggering till {@link #internalExecute(StrolchTransaction)} returns false.</p>
	 *
	 * <p>This occurs when the {@link Action} which is executed, has state set to {@link State#EXECUTED} instead of
	 * {@link State#EXECUTION}. Thus the execution thread stays with this activity, keeping resources bound to it, till
	 * we can wait and allow other activities to execute</p>
	 *
	 * @param tx
	 * 		the TX
	 */
	protected void triggerExecute(StrolchTransaction tx) {
		boolean trigger;
		do {
			trigger = internalExecute(tx);
		} while (trigger);
	}

	/**
	 * Sets the state of the {@link Action} with the given {@link Locator} to {@link State#STOPPED}
	 *
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action} to set to stopped
	 */
	public void toStopped(Locator actionLoc) throws Exception {
		runAsAgent(ctx -> {
			try (StrolchTransaction tx = openTx(ctx.getCertificate())) {
				lockWithRetries(tx);
				if (!refreshActivity(tx))
					return;

				Action action = this.activity.getElementByLocator(actionLoc);
				toStopped(tx, action);

				tx.commitOnClose();
			}
		});

		updateObservers();
	}

	/**
	 * Sets the state of the {@link Action} to {@link State#STOPPED}
	 *
	 * @param tx
	 * 		the TX
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action} to set to stopped
	 */
	public void toStopped(StrolchTransaction tx, Locator actionLoc) {
		lockWithRetries(tx);
		if (!refreshActivity(tx))
			throw new IllegalStateException("Activity " + actionLoc.trim(3) + " does not exist anymore!");
		Action action = this.activity.getElementByLocator(actionLoc);
		toStopped(tx, action);
	}

	public void toStopped(StrolchTransaction tx, Action action) {
		SetActionToStoppedCommand command = new SetActionToStoppedCommand(tx);
		command.setExecutionPolicy(refreshExecutionPolicy(tx, action));
		command.setAction(action);
		command.validate();
		command.doCommand();
	}

	/**
	 * Sets the state of the {@link Action} with the given {@link Locator} to {@link State#ERROR}
	 *
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action} to set to executed
	 */
	public void toError(Locator actionLoc) throws Exception {
		runAsAgent(ctx -> {
			try (StrolchTransaction tx = openTx(ctx.getCertificate())) {
				if (invalidActionContext(tx, actionLoc))
					return;

				Action action = this.activity.getElementByLocator(actionLoc);
				internalToError(tx, action);

				tx.commitOnClose();
			}
		});

		updateObservers();
	}

	/**
	 * Sets the state of the {@link Action} to {@link State#ERROR}
	 *
	 * @param tx
	 * 		the TX
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action} to set to error
	 */
	public void toError(StrolchTransaction tx, Locator actionLoc) {
		if (invalidActionContext(tx, actionLoc))
			return;

		Action action = this.activity.getElementByLocator(actionLoc);
		internalToError(tx, action);
	}

	protected void internalToError(StrolchTransaction tx, Action action) {
		SetActionToErrorCommand command = new SetActionToErrorCommand(tx);
		command.setExecutionPolicy(refreshExecutionPolicy(tx, action));
		command.setAction(action);
		command.validate();
		command.doCommand();
	}

	/**
	 * Sets the state of the {@link Action} with the given {@link Locator} to {@link State#WARNING}
	 *
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action} to set to warning
	 */
	public void toWarning(Locator actionLoc) throws Exception {
		runAsAgent(ctx -> {
			try (StrolchTransaction tx = openTx(ctx.getCertificate())) {
				if (invalidActionContext(tx, actionLoc))
					return;

				Action action = this.activity.getElementByLocator(actionLoc);

				// set this action to warning
				internalToWarning(tx, action);

				tx.commitOnClose();
			}
		});

		updateObservers();
	}

	/**
	 * Sets the state of the {@link Action} with the given {@link Locator} to {@link State#WARNING}
	 *
	 * @param tx
	 * 		the TX
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action} to set to error
	 */
	public void toWarning(StrolchTransaction tx, Locator actionLoc) {
		if (invalidActionContext(tx, actionLoc))
			return;

		Action action = this.activity.getElementByLocator(actionLoc);
		internalToWarning(tx, action);
	}

	protected void internalToWarning(StrolchTransaction tx, Action action) {
		SetActionToWarningCommand command = new SetActionToWarningCommand(tx);
		command.setExecutionPolicy(refreshExecutionPolicy(tx, action));
		command.setAction(action);
		command.validate();
		command.doCommand();
	}

	protected void lockWithRetries(StrolchTransaction tx) throws StrolchLockException {
		if (tx.hasLock(this.locator))
			return;

		int tries = 0;
		while (true) {
			try {

				tx.lock(this.locator);
				return;

			} catch (StrolchLockException e) {
				tries++;
				if (tries >= this.lockRetries) {
					logger.error("Failed to lock " + this.locator + ". Max retries " + tries
							+ " reached, throwing exception!");
					throw e;
				}

				logger.error("LOCK FAILURE!");
				logger.error("Failed to lock " + this.locator + ". Trying again...");
				logger.error("LOCK FAILURE!");
			}
		}
	}

	public void updateObservers() {
		StrolchRealm realm = this.executionHandler.getContainer().getRealm(this.realm);
		if (!realm.isUpdateObservers())
			return;

		ObserverEvent observerEvent = new ObserverEvent();
		observerEvent.updated.addElement(Tags.CONTROLLER, this.activity);
		realm.getObserverHandler().notify(observerEvent);
	}

	@Override
	public String toString() {
		return "Controller{" + "realm='" + realm + '\'' + ", locator=" + locator + '}';
	}
}
