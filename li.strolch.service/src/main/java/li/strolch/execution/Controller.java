package li.strolch.execution;

import static java.util.Collections.synchronizedMap;
import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;

import java.util.HashMap;
import java.util.Map;
import java.util.ResourceBundle;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.ObserverEvent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.execution.command.*;
import li.strolch.execution.policy.ExecutionPolicy;
import li.strolch.handler.operationslog.LogMessage;
import li.strolch.handler.operationslog.LogSeverity;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.Locator;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.Tags;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.privilege.PrivilegedRunnable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Controller {

	private static final Logger logger = LoggerFactory.getLogger(Controller.class);

	private final String realm;
	private ComponentContainer container;
	private ExecutionHandler executionHandler;

	private final String activityType;
	private final String activityId;
	private final Locator locator;

	private Activity activity;

	private Map<Locator, ExecutionPolicy> inExecution;

	public Controller(String realm, ExecutionHandler executionHandler, Activity activity) {
		this.realm = realm;
		this.container = executionHandler.getContainer();
		this.executionHandler = executionHandler;
		this.locator = activity.getLocator();
		this.activityType = activity.getType();
		this.activityId = activity.getId();
		this.activity = activity;
		this.inExecution = synchronizedMap(new HashMap<>());
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

	public ExecutionPolicy getExecutionPolicy(StrolchTransaction tx, Action action) {
		ExecutionPolicy executionPolicy = this.inExecution.computeIfAbsent(action.getLocator(), e -> {
			Resource resource = tx.getResourceFor(action, true);
			return tx.getPolicy(resource.getPolicyDef(ExecutionPolicy.class));
		});

		// always update the TX and controller
		executionPolicy.setController(tx, this);
		return executionPolicy;
	}

	public void removeExecutionPolicy(Action action) {
		this.inExecution.remove(action.getLocator());
	}

	protected StrolchTransaction openTx(Certificate cert) {
		return this.executionHandler.openTx(this.realm, cert, getClass(), false);
	}

	protected void runAsAgent(PrivilegedRunnable runnable) throws PrivilegeException, Exception {
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
				tx.lock(this.locator);
				trigger[0] = execute(tx);
				if (tx.needsCommit()) {
					tx.commitOnClose();
				}
			}
		});

		if (trigger[0])
			this.executionHandler.triggerExecution(this.realm);
	}

	private boolean execute(StrolchTransaction tx) {
		if (!refreshActivity(tx))
			return false;

		if (this.activity.getState().isExecuted()) {
			this.executionHandler.removeFromExecution(this);
			logger.info("Archiving executed activity " + this.locator + " with state " + this.activity.getState());
			this.executionHandler.archiveActivity(this.realm, this.activity);

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
				tx.lock(this.locator);

				if (!refreshActivity(tx))
					return;

				Action action = this.activity.getElementByLocator(actionLoc);

				// set this action to executed
				SetActionToExecutedCommand command = new SetActionToExecutedCommand(tx);
				command.setExecutionPolicy(getExecutionPolicy(tx, action));
				command.setAction(action);
				command.validate();
				command.doCommand();

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
	 * Sets the state of the {@link Action} with the given {@link Locator} to {@link State#STOPPED}
	 *
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action}
	 */
	public void toStopped(Locator actionLoc) throws Exception {
		this.executionHandler.runAsAgent(ctx -> {
			try (StrolchTransaction tx = openTx(ctx.getCertificate())) {
				tx.lock(this.locator);

				if (!refreshActivity(tx))
					return;

				Action action = this.activity.getElementByLocator(actionLoc);

				// set this action to executed
				SetActionToStoppedCommand command = new SetActionToStoppedCommand(tx);
				command.setExecutionPolicy(getExecutionPolicy(tx, action));
				command.setAction(action);
				command.validate();
				command.doCommand();

				tx.commitOnClose();
			}
		});

		updateObservers();
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
				tx.lock(this.locator);

				if (!refreshActivity(tx))
					return;

				Action action = this.activity.getElementByLocator(actionLoc);

				// set this action to executed
				SetActionToErrorCommand command = new SetActionToErrorCommand(tx);
				command.setExecutionPolicy(getExecutionPolicy(tx, action));
				command.setAction(action);
				command.validate();
				command.doCommand();

				tx.commitOnClose();
			}
		});

		updateObservers();
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
				tx.lock(this.locator);

				if (!refreshActivity(tx))
					return;

				Action action = this.activity.getElementByLocator(actionLoc);

				// set this action to executed
				SetActionToWarningCommand command = new SetActionToWarningCommand(tx);
				command.setExecutionPolicy(getExecutionPolicy(tx, action));
				command.setAction(action);
				command.validate();
				command.doCommand();

				tx.commitOnClose();
			}
		});

		updateObservers();
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
									ResourceBundle.getBundle("strolch-service"), "execution.handler.failed.error")
									.withException(e).value("reason", e));
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
									ResourceBundle.getBundle("strolch-service"), "execution.handler.failed.warning")
									.withException(e).value("reason", e));
				}
			}
		});
	}

	private void updateObservers() {
		StrolchRealm realm = this.executionHandler.getContainer().getRealm(this.realm);
		if (!realm.isUpdateObservers())
			return;

		ObserverEvent observerEvent = new ObserverEvent();
		observerEvent.updated.addElement(Tags.CONTROLLER, this.activity);
		realm.getObserverHandler().notify(observerEvent);
	}
}
