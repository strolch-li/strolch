package li.strolch.execution;

import java.util.ResourceBundle;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.execution.command.ExecuteActivityCommand;
import li.strolch.execution.command.SetActionToErrorCommand;
import li.strolch.execution.command.SetActionToExecutedCommand;
import li.strolch.execution.command.SetActionToStoppedCommand;
import li.strolch.execution.command.SetActionToWarningCommand;
import li.strolch.execution.policy.ActivityArchivalPolicy;
import li.strolch.execution.policy.ExecutionPolicy;
import li.strolch.handler.operationslog.LogMessage;
import li.strolch.handler.operationslog.LogSeverity;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.Locator;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.model.policy.PolicyDef;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.PolicyHandler;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.ThreadPoolFactory;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.utils.collections.MapOfSets;
import li.strolch.utils.dbc.DBC;

/**
 * The event based execution handler waits for events in that the {@link ExecutionPolicy} implementations must call the
 * relevant methods when the work is complete. Afterwards the next {@link Action} in the procedure is executed
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class EventBasedExecutionHandler extends ExecutionHandler {

	private static final String KEY_DEFAULT_ACTIVITY_ARCHIVAL = "key:DefaultActivityArchival";

	private ExecutorService executorService;

	private MapOfSets<String, Locator> registeredActivities;

	private DelayedExecutionTimer delayedExecutionTimer;

	public EventBasedExecutionHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {

		this.registeredActivities = new MapOfSets<>();

		super.initialize(configuration);
	}

	@Override
	public void start() throws Exception {

		this.executorService = Executors.newCachedThreadPool(new ThreadPoolFactory("ExecutionHandler"));
		this.delayedExecutionTimer = new SimpleDurationExecutionTimer();

		super.start();
	}

	@Override
	public void stop() throws Exception {

		if (this.executorService != null) {
			this.executorService.shutdown();
			while (!this.executorService.isTerminated()) {
				logger.info("Waiting for executor service to terminate...");
				Thread.sleep(50L);
			}
			this.executorService = null;
		}

		if (this.delayedExecutionTimer != null) {
			this.delayedExecutionTimer.destroy();
			this.delayedExecutionTimer = null;
		}

		super.stop();
	}

	@Override
	public void addForExecution(String realm, Locator activityLoc) {
		Locator rootElemLoc = activityLoc.trim(3);
		synchronized (this.registeredActivities) {
			this.registeredActivities.addElement(realm, rootElemLoc);
		}
		toExecution(realm, activityLoc);
	}

	@Override
	public void removeFromExecution(String realm, Locator activityLoc) {
		Locator rootElemLoc = activityLoc.trim(3);
		synchronized (this.registeredActivities) {
			this.registeredActivities.removeElement(realm, rootElemLoc);
		}
	}

	@Override
	public void triggerExecution(String realm) {
		synchronized (this.registeredActivities) {
			Set<Locator> locators = this.registeredActivities.getSet(realm);
			if (locators != null) {
				for (Locator locator : locators) {
					// execute async
					toExecution(realm, locator);
				}
			}
		}
	}

	@Override
	public void toExecution(String realm, Locator locator) {
		this.executorService.execute(() -> {
			try {
				runAsAgent(ctx -> {
					toExecution(realm, locator, ctx);
				});
			} catch (Exception e) {
				logger.error("Failed to set " + locator + " to execution due to " + e.getMessage(), e);

				if (getContainer().hasComponent(OperationsLog.class)) {
					getComponent(OperationsLog.class).addMessage(new LogMessage(realm, locator, LogSeverity.EXCEPTION,
							ResourceBundle.getBundle("strolch-service"), "execution.handler.failed.execution")
									.value("reason", e));
				}
			}
		});
	}

	@Override
	public void toExecuted(String realm, Locator locator) {
		this.executorService.execute(() -> {
			try {
				runAsAgent(ctx -> {
					toExecuted(realm, locator, ctx);
				});
			} catch (Exception e) {
				logger.error("Failed to set " + locator + " to executed due to " + e.getMessage(), e);

				if (getContainer().hasComponent(OperationsLog.class)) {
					getComponent(OperationsLog.class).addMessage(new LogMessage(realm, locator, LogSeverity.EXCEPTION,
							ResourceBundle.getBundle("strolch-service"), "execution.handler.failed.executed")
									.value("reason", e));
				}
			}
		});
	}

	@Override
	public void toStopped(String realm, Locator locator) {
		this.executorService.execute(() -> {
			try {
				runAsAgent(ctx -> {
					toStopped(realm, locator, ctx);
				});
			} catch (Exception e) {
				logger.error("Failed to set " + locator + " to stopped due to " + e.getMessage(), e);

				if (getContainer().hasComponent(OperationsLog.class)) {
					getComponent(OperationsLog.class).addMessage(new LogMessage(realm, locator, LogSeverity.EXCEPTION,
							ResourceBundle.getBundle("strolch-service"), "execution.handler.failed.stopped")
									.value("reason", e));
				}
			}
		});
	}

	@Override
	public void toError(String realm, Locator locator) {
		this.executorService.execute(() -> {
			try {
				runAsAgent(ctx -> {
					toError(realm, locator, ctx);
				});
			} catch (Exception e) {
				logger.error("Failed to set " + locator + " to error due to " + e.getMessage(), e);

				if (getContainer().hasComponent(OperationsLog.class)) {
					getComponent(OperationsLog.class).addMessage(new LogMessage(realm, locator, LogSeverity.EXCEPTION,
							ResourceBundle.getBundle("strolch-service"), "execution.handler.failed.error")
									.value("reason", e));
				}
			}
		});
	}

	@Override
	public void toWarning(String realm, Locator locator) {
		this.executorService.execute(() -> {
			try {
				runAsAgent(ctx -> {
					toWarning(realm, locator, ctx);
				});
			} catch (Exception e) {
				logger.error("Failed to set " + locator + " to warning due to " + e.getMessage(), e);

				if (getContainer().hasComponent(OperationsLog.class)) {
					getComponent(OperationsLog.class).addMessage(new LogMessage(realm, locator, LogSeverity.EXCEPTION,
							ResourceBundle.getBundle("strolch-service"), "execution.handler.failed.warning")
									.value("reason", e));
				}
			}
		});
	}

	private void archiveActivity(String realm, Locator activityLoc) {
		this.executorService.execute(() -> {
			try {
				runAsAgent(ctx -> {
					try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), ActivityArchivalPolicy.class)) {
						tx.lock(activityLoc);

						Activity activity = tx.findElement(activityLoc);

						logger.info("Activity " + activity.getLocator() + " is in state " + activity.getState());

						PolicyDef policyDef;
						if (activity.hasPolicyDef(ActivityArchivalPolicy.class.getSimpleName())) {
							policyDef = activity.getPolicyDef(ActivityArchivalPolicy.class.getSimpleName());
						} else {
							policyDef = PolicyDef.valueOf(ActivityArchivalPolicy.class.getSimpleName(),
									KEY_DEFAULT_ACTIVITY_ARCHIVAL);
						}

						PolicyHandler policyHandler = getComponent(PolicyHandler.class);
						ActivityArchivalPolicy archivalPolicy = policyHandler.getPolicy(policyDef, tx);
						archivalPolicy.archive(activity);

					}
				});
			} catch (Exception e) {
				logger.error("Failed to archive " + activityLoc + " due to " + e.getMessage(), e);

				if (getContainer().hasComponent(OperationsLog.class)) {
					getComponent(OperationsLog.class)
							.addMessage(new LogMessage(realm, activityLoc, LogSeverity.EXCEPTION,
									ResourceBundle.getBundle("strolch-service"), "execution.handler.failed.archive")
											.value("reason", e));
				}
			}
		});
	}

	private void toExecution(String realm, Locator elementLoc, PrivilegeContext ctx) {
		try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), ExecuteActivityCommand.class)) {

			Locator activityLoc = elementLoc.trim(3);
			tx.lock(activityLoc);

			Activity activity = tx.findElement(activityLoc, true);
			if (activity == null) {
				logger.error("Element for locator " + elementLoc + " does not exist!");
				synchronized (this.registeredActivities) {
					this.registeredActivities.removeElement(realm, activityLoc);
				}
				return;
			}

			ExecuteActivityCommand command = new ExecuteActivityCommand(getContainer(), tx);
			command.setActivity(activity);
			tx.addCommand(command);

			tx.commitOnClose();
		}
	}

	private void toExecuted(String realm, Locator actionLoc, PrivilegeContext ctx) {

		Locator activityLoc = actionLoc.trim(3);

		try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), SetActionToExecutedCommand.class)) {

			tx.lock(activityLoc);

			Action action = tx.findElement(actionLoc);

			// set this action to executed
			SetActionToExecutedCommand command = new SetActionToExecutedCommand(getContainer(), tx);
			command.setAction(action);
			tx.addCommand(command);

			// flush so we can see that changes performed
			tx.flush();

			// if the activity is now executed, remove it from the registered activities
			Activity activity = action.getRootElement();
			if (activity.getState().isExecuted()) {

				synchronized (this.registeredActivities) {
					if (!this.registeredActivities.removeElement(realm, activityLoc))
						logger.warn("Activity " + activityLoc + " already removed from registered activities!");
				}

				archiveActivity(realm, activity.getLocator());

			} else {

				// otherwise execute any next action(s) for this action's activity

				ExecuteActivityCommand execCommand = new ExecuteActivityCommand(getContainer(), tx);
				execCommand.setActivity(activity);
				tx.addCommand(execCommand);

				// flush so we can see that changes performed
				tx.flush();
			}

			tx.commitOnClose();
		}

		// now trigger a further execution of any other activities needed execution in this realm
		triggerExecution(realm);
	}

	private void toWarning(String realm, Locator actionLoc, PrivilegeContext ctx) {
		try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), SetActionToExecutedCommand.class)) {
			Locator rootElemLoc = actionLoc.trim(3);
			tx.lock(rootElemLoc);

			IActivityElement elem = tx.findElement(actionLoc);
			DBC.INTERIM.assertEquals("toWarning only for Action!", Action.class, elem.getClass());

			SetActionToWarningCommand command = new SetActionToWarningCommand(getContainer(), tx);
			command.setAction((Action) elem);
			tx.addCommand(command);

			tx.commitOnClose();
		}
	}

	private void toError(String realm, Locator actionLoc, PrivilegeContext ctx) {
		try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), SetActionToExecutedCommand.class)) {
			Locator rootElemLoc = actionLoc.trim(3);
			tx.lock(rootElemLoc);

			IActivityElement elem = tx.findElement(actionLoc);
			DBC.INTERIM.assertEquals("toError only for Action!", Action.class, elem.getClass());

			SetActionToErrorCommand command = new SetActionToErrorCommand(getContainer(), tx);
			command.setAction((Action) elem);
			tx.addCommand(command);

			tx.commitOnClose();
		}
	}

	private void toStopped(String realm, Locator actionLoc, PrivilegeContext ctx) {
		try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), SetActionToStoppedCommand.class)) {
			Locator rootElemLoc = actionLoc.trim(3);
			tx.lock(rootElemLoc);

			IActivityElement elem = tx.findElement(actionLoc);
			DBC.INTERIM.assertEquals("toStopped only for Action!", Action.class, elem.getClass());

			SetActionToStoppedCommand command = new SetActionToStoppedCommand(getContainer(), tx);
			command.setAction((Action) elem);
			tx.addCommand(command);

			tx.commitOnClose();
		}

		// now trigger a further execution of any other activities needed execution in this realm
		triggerExecution(realm);
	}

	protected StrolchTransaction openTx(String realm, Certificate cert, Class<?> clazz) {
		return getContainer().getRealm(realm).openTx(cert, clazz);
	}

	@Override
	public DelayedExecutionTimer getDelayedExecutionTimer() {
		return this.delayedExecutionTimer;
	}
}
