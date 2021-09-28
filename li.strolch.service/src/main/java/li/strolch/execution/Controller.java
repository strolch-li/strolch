package li.strolch.execution;

import static java.util.Collections.synchronizedMap;
import static li.strolch.execution.EventBasedExecutionHandler.PROP_LOCK_RETRIES;
import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;

import java.util.HashMap;
import java.util.Map;
import java.util.ResourceBundle;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.ObserverEvent;
import li.strolch.agent.api.StrolchLockException;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.execution.command.*;
import li.strolch.execution.policy.ExecutionPolicy;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.Locator;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.Tags;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogMessageState;
import li.strolch.model.log.LogSeverity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.privilege.PrivilegedRunnable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Controller {

	private static final Logger logger = LoggerFactory.getLogger(Controller.class);

	private final int lockRetries;
	private final String realm;
	private final ComponentContainer container;
	private final ExecutionHandler executionHandler;

	private final String activityType;
	private final String activityId;
	private final Locator locator;

	private Activity activity;

	private final Map<Locator, ExecutionPolicy> inExecution;

	public Controller(String realm, ExecutionHandler executionHandler, Activity activity) {
		this.realm = realm;
		this.container = executionHandler.getContainer();
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

	public ExecutionPolicy refreshExecutionPolicy(StrolchTransaction tx, Action action) {
		ExecutionPolicy executionPolicy = this.inExecution.computeIfAbsent(action.getLocator(), e -> {
			Resource resource = tx.getResourceFor(action, true);
			return tx.getPolicy(resource, ExecutionPolicy.class);
		});

		// always update the TX and controller
		executionPolicy.setController(tx, this);
		executionPolicy.setStopped(false);
		return executionPolicy;
	}

	public void removeExecutionPolicy(Action action) {
		this.inExecution.remove(action.getLocator());
	}

	protected StrolchTransaction openTx(Certificate cert) {
		return this.executionHandler.openTx(this.realm, cert, getClass(), false);
	}

	protected void runAsAgent(PrivilegedRunnable runnable) throws Exception {
		this.executionHandler.runAsAgent(runnable);
	}

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
	 * Starts the execution of this {@link Activity}
	 */
	public void execute() throws Exception {
		boolean[] trigger = new boolean[1];
		this.executionHandler.runAsAgent(ctx -> {
			try (StrolchTransaction tx = openTx(ctx.getCertificate())) {
				lockWithRetries(tx);
				trigger[0] = execute(tx);
				if (tx.needsCommit()) {
					tx.commitOnClose();
				}
			}
		});

		if (trigger[0])
			this.executionHandler.triggerExecution(this.realm);
	}

	/**
	 * Stops the execution of all actions
	 */
	public void stop() {
		synchronized (this.inExecution) {
			this.inExecution.values().forEach(ExecutionPolicy::stop);
		}
	}

	private boolean execute(StrolchTransaction tx) {
		if (!refreshActivity(tx))
			return false;

		if (this.activity.getState().isExecuted()) {
			this.executionHandler.removeFromExecution(this);
			logger.info("Archiving executed activity " + this.locator + " with state " + this.activity.getState());
			this.executionHandler.archiveActivity(this.realm, this.activity.getLocator());

			return false;
		}

		ExecutionHandlerState state = this.executionHandler.getState(this.realm);
		if (state == ExecutionHandlerState.Paused) {
			logger.warn("Ignoring trigger for paused realm " + this.realm);
			return false;
		}

		ExecuteActivityCommand command = new ExecuteActivityCommand(tx);
		command.setController(this);
		command.validate();
		command.doCommand();

		updateObservers();

		return command.needsRetriggerOfExecution();
	}

	/**
	 * Completes the execution of the given {@link Action} with the given {@link Locator}
	 *
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action}
	 */
	public void toExecuted(Locator actionLoc) throws Exception {
		this.executionHandler.runAsAgent(ctx -> {
			try (StrolchTransaction tx = openTx(ctx.getCertificate())) {
				lockWithRetries(tx);

				if (!refreshActivity(tx))
					return;

				Action action = this.activity.getElementByLocator(actionLoc);

				// set this action to executed
				toExecuted(tx, action);

				updateObservers();

				// flush so we can see the changes performed
				tx.flush();

				// now try and execute the next action(s)
				execute(tx);

				if (tx.needsCommit())
					tx.commitOnClose();
			}
		});

		this.executionHandler.triggerExecution(this.realm);
	}

	/**
	 * Completes the execution of the given {@link Action}
	 *
	 * @param tx
	 * 		the TX
	 * @param action
	 * 		the {@link Action} to set to executed
	 */
	public void toExecuted(StrolchTransaction tx, Action action) throws Exception {
		SetActionToExecutedCommand command = new SetActionToExecutedCommand(tx);
		command.setExecutionPolicy(refreshExecutionPolicy(tx, action));
		command.setAction(action);
		command.validate();
		command.doCommand();
	}

	/**
	 * Sets the state of the {@link Action} with the given {@link Locator} to {@link State#STOPPED}
	 *
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action}
	 */
	public void toStopped(Locator actionLoc) throws Exception {
		this.executionHandler.runAsAgent(ctx -> {
			try (StrolchTransaction tx = openTx(ctx.getCertificate())) {
				lockWithRetries(tx);

				if (!refreshActivity(tx))
					return;

				Action action = this.activity.getElementByLocator(actionLoc);

				// set this action to executed
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
	 * @param action
	 * 		the {@link Action} to set to stopped
	 */
	public void toStopped(StrolchTransaction tx, Action action) throws Exception {
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
	 * 		the {@link Locator} of the {@link Action}
	 */
	public void toError(Locator actionLoc) throws Exception {
		this.executionHandler.runAsAgent(ctx -> {
			try (StrolchTransaction tx = openTx(ctx.getCertificate())) {
				lockWithRetries(tx);

				if (!refreshActivity(tx))
					return;

				Action action = this.activity.getElementByLocator(actionLoc);

				// set this action to error
				toError(tx, action);

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
	 * @param action
	 * 		the {@link Action} to set to error
	 */
	public void toError(StrolchTransaction tx, Action action) throws Exception {
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
	 * 		the {@link Locator} of the {@link Action}
	 */
	public void toWarning(Locator actionLoc) throws Exception {
		this.executionHandler.runAsAgent(ctx -> {
			try (StrolchTransaction tx = openTx(ctx.getCertificate())) {
				lockWithRetries(tx);

				if (!refreshActivity(tx))
					return;

				Action action = this.activity.getElementByLocator(actionLoc);

				// set this action to warning
				toWarning(tx, action);

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
	 * @param action
	 * 		the {@link Action} to set to error
	 */
	public void toWarning(StrolchTransaction tx, Action action) throws Exception {
		SetActionToWarningCommand command = new SetActionToWarningCommand(tx);
		command.setExecutionPolicy(refreshExecutionPolicy(tx, action));
		command.setAction(action);
		command.validate();
		command.doCommand();
	}

	/**
	 * Sets the state of the {@link Action} with the given {@link Locator} to {@link State#ERROR}
	 *
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action}
	 */
	public void asyncToError(Locator actionLoc) {
		this.executionHandler.getExecutor().submit(() -> {
			try {
				toError(actionLoc);
			} catch (Exception e) {
				logger.error("Failed to set " + locator + " to error due to " + e.getMessage(), e);

				if (this.container.hasComponent(OperationsLog.class)) {
					this.container.getComponent(OperationsLog.class).addMessage(
							new LogMessage(realm, SYSTEM_USER_AGENT, locator, LogSeverity.Exception,
									LogMessageState.Information, ResourceBundle.getBundle("strolch-service"),
									"execution.handler.failed.error").withException(e).value("reason", e));
				}
			}
		});
	}

	/**
	 * Sets the state of the {@link Action} with the given {@link Locator} to {@link State#WARNING}
	 *
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action}
	 */
	public void asyncToWarning(Locator actionLoc) {
		this.executionHandler.getExecutor().submit(() -> {
			try {
				toWarning(actionLoc);
			} catch (Exception e) {
				logger.error("Failed to set " + locator + " to warning due to " + e.getMessage(), e);

				if (this.container.hasComponent(OperationsLog.class)) {
					this.container.getComponent(OperationsLog.class).addMessage(
							new LogMessage(realm, SYSTEM_USER_AGENT, locator, LogSeverity.Exception,
									LogMessageState.Information, ResourceBundle.getBundle("strolch-service"),
									"execution.handler.failed.warning").withException(e).value("reason", e));
				}
			}
		});
	}

	private void lockWithRetries(StrolchTransaction tx) throws StrolchLockException {
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
}
