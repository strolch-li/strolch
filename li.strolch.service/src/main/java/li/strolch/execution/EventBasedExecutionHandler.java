package li.strolch.execution;

import static java.util.Collections.emptySet;
import static li.strolch.model.StrolchModelConstants.*;
import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;
import static li.strolch.utils.collections.SynchronizedCollections.synchronizedMapOfMaps;

import java.util.*;
import java.util.concurrent.ExecutorService;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.ObserverEvent;
import li.strolch.execution.command.*;
import li.strolch.execution.policy.ExecutionPolicy;
import li.strolch.handler.operationslog.LogMessage;
import li.strolch.handler.operationslog.LogSeverity;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.*;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.utils.collections.MapOfMaps;
import li.strolch.utils.dbc.DBC;

/**
 * The event based execution handler waits for events in that the {@link ExecutionPolicy} implementations must call the
 * relevant methods when the work is complete. Afterwards the next {@link Action} in the procedure is executed
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class EventBasedExecutionHandler extends ExecutionHandler {

	private static final String PROP_RESTART_EXECUTION = "restartExecution";

	private Map<String, ExecutionHandlerState> statesByRealm;
	private MapOfMaps<String, Locator, Controller> controllers;

	private DelayedExecutionTimer delayedExecutionTimer;

	public EventBasedExecutionHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	private ExecutorService getExecutor() {
		return getExecutorService("ExecutionHandler");
	}

	@Override
	public Set<Locator> getActiveActivitiesLocator(String realm) {
		if (this.controllers == null)
			return emptySet();
		Map<Locator, Controller> activities = this.controllers.getMap(realm);
		if (activities == null)
			return emptySet();
		return activities.keySet();
	}

	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {
		this.controllers = synchronizedMapOfMaps(new MapOfMaps<>());
		super.initialize(configuration);
	}

	@Override
	public void start() throws Exception {

		evaluateStateByRealm();

		this.delayedExecutionTimer = new SimpleDurationExecutionTimer(getContainer().getAgent());

		// restart execution of activities already in execution
		if (!getConfiguration().getBoolean(PROP_RESTART_EXECUTION, Boolean.FALSE)) {
			logger.info("Not restarting execution of activities.");
		} else {
			logger.info("Restarting execution of activities.");
			runAsAgent(this::restartActivityExecution);
		}

		super.start();
	}

	@Override
	public void stop() throws Exception {

		if (this.delayedExecutionTimer != null) {
			this.delayedExecutionTimer.destroy();
			this.delayedExecutionTimer = null;
		}

		super.stop();
	}

	@Override
	public void addForExecution(String realm, Activity activity) {
		ExecutionHandlerState state = this.statesByRealm.getOrDefault(realm, ExecutionHandlerState.Running);
		if (state == ExecutionHandlerState.HaltNew)
			throw new IllegalStateException(
					"ExecutionHandler state is " + state + ", can not add activities for execution!");

		Controller controller = new Controller(this, activity);
		this.controllers.addElement(realm, activity.getLocator(), controller);
		notifyObserverAdd(realm, controller);
		toExecution(realm, activity);
	}

	@Override
	public void removeFromExecution(String realm, Locator activityLoc) {
		Locator rootElemLoc = activityLoc.trim(3);
		Controller controller = this.controllers.removeElement(realm, rootElemLoc);
		if (controller != null)
			getExecutor().submit(() -> notifyObserverRemove(realm, controller));
	}

	@Override
	public void clearAllCurrentExecutions(String realm) {
		Map<Locator, Controller> removed = this.controllers.removeMap(realm);
		getExecutor().submit(() -> notifyObserverRemove(realm, removed));
	}

	private void restartActivityExecution(PrivilegeContext ctx) {
		// iterate the realms
		for (String realmName : getContainer().getRealmNames()) {
			reloadActivitiesInExecution(ctx, realmName);
		}
	}

	@Override
	public void reloadActivitiesInExecution(PrivilegeContext ctx, String realmName) {
		try (StrolchTransaction tx = openTx(realmName, ctx.getCertificate(), false)) {

			// iterate all activities
			tx.streamActivities().forEach(activity -> {

				if (activity.isReadOnly())
					activity = activity.getClone(true);

				// we only want to restart activities which were in execution
				State state = activity.getState();
				if (!state.inExecutionPhase())
					return;

				logger.info("Starting Execution of " + activity.getLocator() + " on realm " + realmName);

				// Activities need to be in state STOPPED to restart
				if (state == State.ERROR) {
					activity.getActionsWithState(State.ERROR).forEach(a -> a.setState(State.STOPPED));
				} else if (state == State.WARNING) {
					activity.getActionsWithState(State.WARNING).forEach(a -> a.setState(State.STOPPED));
				} else if (state == State.EXECUTION) {
					activity.getActionsWithState(State.EXECUTION).forEach(a -> a.setState(State.STOPPED));
				}
				tx.update(activity);

				// register for execution
				Controller controller = new Controller(this, activity);
				this.controllers.addElement(realmName, activity.getLocator(), controller);
			});

			// commit changes to state
			tx.commitOnClose();
		}

		// trigger execution of the registered activities
		triggerExecution(realmName);
	}

	@Override
	public void triggerExecution(String realm) {

		ExecutionHandlerState state = this.statesByRealm.getOrDefault(realm, ExecutionHandlerState.Running);
		if (state == ExecutionHandlerState.Paused) {
			logger.warn("Ignoring trigger for paused realm " + realm);
			return;
		}

		synchronized (this.controllers) {
			Map<Locator, Controller> controllers = this.controllers.getMap(realm);
			if (controllers != null) {
				for (Controller controller : controllers.values()) {
					// execute async
					toExecution(realm, controller);
				}
			}
		}
	}

	@Override
	public ExecutionHandlerState getState(String realm) {
		return this.statesByRealm.getOrDefault(realm, ExecutionHandlerState.Running);
	}

	private void evaluateStateByRealm() throws Exception {

		this.statesByRealm = Collections.synchronizedMap(new HashMap<>());

		runAsAgent(ctx -> getContainer().getRealmNames().forEach(realm -> {
			try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), false)) {
				Resource executionHandlerConfig = tx
						.getResourceBy(TYPE_CONFIGURATION, ExecutionHandler.class.getSimpleName());
				if (executionHandlerConfig == null) {
					this.statesByRealm.put(realm, ExecutionHandlerState.Running);
				} else {
					ParameterBag parameters = executionHandlerConfig.getParameterBag(BAG_PARAMETERS);
					if (parameters == null) {
						this.statesByRealm.put(realm, ExecutionHandlerState.Running);
					} else {
						StringParameter stateP = parameters.getParameter(PARAM_STATE);
						if (stateP == null) {
							this.statesByRealm.put(realm, ExecutionHandlerState.Running);
						} else {
							ExecutionHandlerState state;
							try {
								state = ExecutionHandlerState.valueOf(stateP.getValue());
							} catch (Exception e) {
								state = ExecutionHandlerState.Running;
								stateP.setValue(ExecutionHandlerState.Running.name());
								tx.update(executionHandlerConfig);
								tx.commitOnClose();
								logger.error("Failed to read unhandled state " + stateP.getValue(), e);
							}
							this.statesByRealm.put(realm, state);
						}
					}
				}
			}
		}));
	}

	@Override
	public void setState(Certificate cert, String realm, ExecutionHandlerState state) {
		try (StrolchTransaction tx = openTx(realm, cert, false)) {
			Resource executionHandlerConfig = tx
					.getResourceBy(TYPE_CONFIGURATION, ExecutionHandler.class.getSimpleName());
			if (executionHandlerConfig == null) {
				executionHandlerConfig = new Resource(ExecutionHandler.class.getSimpleName(),
						"ExecutionHandler Configuration", TYPE_CONFIGURATION);
			}
			ParameterBag parameters = executionHandlerConfig.getParameterBag(BAG_PARAMETERS);
			if (parameters == null) {
				parameters = new ParameterBag(BAG_PARAMETERS, "Parameters", TYPE_PARAMETERS);
				executionHandlerConfig.addParameterBag(parameters);
			}
			StringParameter stateP = parameters.getParameter(PARAM_STATE);
			if (stateP == null) {
				stateP = new StringParameter(PARAM_STATE, "State", state);
				parameters.addParameter(stateP);
			}

			stateP.setValueE(state);

			tx.addOrUpdate(executionHandlerConfig);
			tx.commitOnClose();

			this.statesByRealm.put(realm, state);
		}
	}

	@Override
	public void toExecution(String realm, Activity activity) {

		ExecutionHandlerState state = this.statesByRealm.getOrDefault(realm, ExecutionHandlerState.Running);
		if (state == ExecutionHandlerState.Paused) {
			logger.warn("Ignoring execution of " + activity.getLocator() + " for paused realm " + realm);
			return;
		}

		getExecutor().execute(() -> {
			try {
				runAsAgent(ctx -> toExecution(realm, activity, ctx));
			} catch (Exception e) {
				logger.error("Failed to set " + activity.getLocator() + " to execution", e);

				if (getContainer().hasComponent(OperationsLog.class)) {
					getComponent(OperationsLog.class).addMessage(
							new LogMessage(realm, SYSTEM_USER_AGENT, activity.getLocator(), LogSeverity.Exception,
									ResourceBundle.getBundle("strolch-service"), "execution.handler.failed.execution")
									.withException(e).value("reason", e));
				}
			}
		});
	}

	@Override
	public void toExecuted(String realm, Locator locator) {
		getExecutor().execute(() -> {
			try {
				runAsAgent(ctx -> toExecuted(realm, locator, ctx));
			} catch (Exception e) {
				logger.error("Failed to set " + locator + " to executed due to " + e.getMessage(), e);

				if (getContainer().hasComponent(OperationsLog.class)) {
					getComponent(OperationsLog.class).addMessage(
							new LogMessage(realm, SYSTEM_USER_AGENT, locator, LogSeverity.Exception,
									ResourceBundle.getBundle("strolch-service"), "execution.handler.failed.executed")
									.withException(e).value("reason", e));
				}
			}
		});
	}

	@Override
	public void toStopped(String realm, Locator locator) {
		getExecutor().execute(() -> {
			try {
				runAsAgent(ctx -> toStopped(realm, locator, ctx));
			} catch (Exception e) {
				logger.error("Failed to set " + locator + " to stopped due to " + e.getMessage(), e);

				if (getContainer().hasComponent(OperationsLog.class)) {
					getComponent(OperationsLog.class).addMessage(
							new LogMessage(realm, SYSTEM_USER_AGENT, locator, LogSeverity.Exception,
									ResourceBundle.getBundle("strolch-service"), "execution.handler.failed.stopped")
									.withException(e).value("reason", e));
				}
			}
		});
	}

	@Override
	public void toError(String realm, Locator locator) {
		getExecutor().execute(() -> {
			try {
				runAsAgent(ctx -> toError(realm, locator, ctx));
			} catch (Exception e) {
				logger.error("Failed to set " + locator + " to error due to " + e.getMessage(), e);

				if (getContainer().hasComponent(OperationsLog.class)) {
					getComponent(OperationsLog.class).addMessage(
							new LogMessage(realm, SYSTEM_USER_AGENT, locator, LogSeverity.Exception,
									ResourceBundle.getBundle("strolch-service"), "execution.handler.failed.error")
									.withException(e).value("reason", e));
				}
			}
		});
	}

	@Override
	public void toWarning(String realm, Locator locator) {
		getExecutor().execute(() -> {
			try {
				runAsAgent(ctx -> toWarning(realm, locator, ctx));
			} catch (Exception e) {
				logger.error("Failed to set " + locator + " to warning due to " + e.getMessage(), e);

				if (getContainer().hasComponent(OperationsLog.class)) {
					getComponent(OperationsLog.class).addMessage(
							new LogMessage(realm, SYSTEM_USER_AGENT, locator, LogSeverity.Exception,
									ResourceBundle.getBundle("strolch-service"), "execution.handler.failed.warning")
									.withException(e).value("reason", e));
				}
			}
		});
	}

	@Override
	public void archiveActivity(String realm, Activity activity) {
		getExecutor().execute(() -> {
			try {
				runAsAgent(ctx -> {
					try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), ArchiveActivityCommand.class,
							false)) {
						ArchiveActivityCommand command = new ArchiveActivityCommand(tx);
						command.setActivityLoc(activity.getLocator());
						tx.addCommand(command);
						tx.commitOnClose();
					}
				});
			} catch (Exception e) {
				logger.error("Failed to archive " + activity.getLocator() + " due to " + e.getMessage(), e);

				if (getContainer().hasComponent(OperationsLog.class)) {
					getComponent(OperationsLog.class).addMessage(
							new LogMessage(realm, SYSTEM_USER_AGENT, activity.getLocator(), LogSeverity.Exception,
									ResourceBundle.getBundle("strolch-service"), "execution.handler.failed.archive")
									.withException(e).value("reason", e));
				}
			}
		});
	}

	private void toExecution(String realm, Locator elementLoc, PrivilegeContext ctx) {
		try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), ExecuteActivityCommand.class, false)) {

			Locator activityLoc = elementLoc.trim(3);
			tx.lock(activityLoc);

			Activity activity = tx.findElement(activityLoc, true);
			if (activity == null) {
				logger.error("Element for locator " + elementLoc + " does not exist!");
				synchronized (this.registeredActivities) {
					this.registeredActivities.removeElement(realm, activityLoc);
				}
				notifyObserverRemove(realm, activityLoc);
				return;
			}

			if (activity.getState().isExecuted()) {

				synchronized (this.registeredActivities) {
					if (!this.registeredActivities.removeElement(realm, activityLoc))
						logger.warn("Activity " + activityLoc + " already removed from registered activities!");
				}

				notifyObserverRemove(tx, activity);

				logger.info("Archiving activity " + activityLoc + " with state " + activity.getState());
				archiveActivity(realm, activity.getLocator());

			} else {

				ExecuteActivityCommand command = new ExecuteActivityCommand(getContainer(), tx);
				command.setActivity(activity);
				command.validate();
				command.doCommand();

				notifyObserverUpdate(tx, activity);
				tx.commitOnClose();
			}
		}
	}

	private void toExecuted(String realm, Locator actionLoc, PrivilegeContext ctx) {

		Locator activityLoc = actionLoc.trim(3);

		try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), SetActionToExecutedCommand.class, false)) {

			tx.lock(activityLoc);

			Action action = tx.findElement(actionLoc);

			// set this action to executed
			SetActionToExecutedCommand command = new SetActionToExecutedCommand(getContainer(), tx);
			command.setAction(action);
			command.validate();
			command.doCommand();

			notifyObserverUpdate(tx, action.getRootElement());

			// flush so we can see the changes performed
			tx.flush();

			// if the activity is now executed, remove it from the registered activities
			Activity activity = action.getRootElement().getClone(true);
			if (activity.getState().isExecuted()) {

				synchronized (this.registeredActivities) {
					if (!this.registeredActivities.removeElement(realm, activityLoc))
						logger.warn("Activity " + activityLoc + " already removed from registered activities!");
				}

				notifyObserverRemove(tx, action.getRootElement());

				logger.info("Archiving activity " + activityLoc + " with state " + activity.getState());
				archiveActivity(realm, activity.getLocator());

			} else {

				ExecutionHandlerState state = this.statesByRealm.getOrDefault(realm, ExecutionHandlerState.Running);
				if (state == ExecutionHandlerState.Paused) {
					logger.warn("Ignoring trigger for paused realm " + realm);
				} else {

					// otherwise execute any next action(s) for this action's activity

					ExecuteActivityCommand execCommand = new ExecuteActivityCommand(getContainer(), tx);
					execCommand.setActivity(activity);
					execCommand.validate();
					execCommand.doCommand();

					notifyObserverUpdate(tx, action.getRootElement());

					// flush so we can see the changes performed
					tx.flush();
				}
			}

			tx.commitOnClose();
		}

		// now trigger a further execution of any other activities needed execution in this realm
		triggerExecution(realm);
	}

	private void toWarning(String realm, Locator actionLoc, PrivilegeContext ctx) {
		try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), SetActionToExecutedCommand.class, false)) {
			Locator rootElemLoc = actionLoc.trim(3);
			tx.lock(rootElemLoc);

			IActivityElement elem = tx.findElement(actionLoc);
			DBC.INTERIM.assertEquals("toWarning only for Action!", Action.class, elem.getClass());

			SetActionToWarningCommand command = new SetActionToWarningCommand(getContainer(), tx);
			command.setAction((Action) elem);
			command.validate();
			command.doCommand();

			notifyObserverUpdate(tx, elem.getRootElement());
			tx.commitOnClose();
		}
	}

	private void toError(String realm, Locator actionLoc, PrivilegeContext ctx) {
		try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), SetActionToExecutedCommand.class, false)) {
			Locator rootElemLoc = actionLoc.trim(3);
			tx.lock(rootElemLoc);

			IActivityElement elem = tx.findElement(actionLoc);
			DBC.INTERIM.assertEquals("toError only for Action!", Action.class, elem.getClass());

			SetActionToErrorCommand command = new SetActionToErrorCommand(getContainer(), tx);
			command.setAction((Action) elem);
			command.validate();
			command.doCommand();

			notifyObserverUpdate(tx, elem.getRootElement());
			tx.commitOnClose();
		}
	}

	private void toStopped(String realm, Locator actionLoc, PrivilegeContext ctx) {
		try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), SetActionToStoppedCommand.class, false)) {
			Locator rootElemLoc = actionLoc.trim(3);
			tx.lock(rootElemLoc);

			IActivityElement elem = tx.findElement(actionLoc);
			DBC.INTERIM.assertEquals("toStopped only for Action!", Action.class, elem.getClass());

			SetActionToStoppedCommand command = new SetActionToStoppedCommand(getContainer(), tx);
			command.setAction((Action) elem);
			command.validate();
			command.doCommand();

			notifyObserverUpdate(tx, elem.getRootElement());
			tx.commitOnClose();
		}

		// now trigger a further execution of any other activities needed execution in this realm
		triggerExecution(realm);
	}

	private void notifyObserverAdd(String realm, Controller controller) {
		if (!getContainer().getRealm(realm).isUpdateObservers())
			return;

		ObserverEvent observerEvent = new ObserverEvent();
		observerEvent.added.addElement(Tags.CONTROLLER, rootElement);
		getContainer().getRealm(realm).getObserverHandler().notify(observerEvent);
	}

	private void notifyObserverUpdate(StrolchTransaction tx, Controller controller) {
		if (!getContainer().getRealm(tx.getRealmName()).isUpdateObservers())
			return;

		ObserverEvent observerEvent = new ObserverEvent();
		observerEvent.updated.addElement(Tags.CONTROLLER, rootElement);
		tx.getContainer().getRealm(tx.getRealmName()).getObserverHandler().notify(observerEvent);
	}

	private void notifyObserverRemove(StrolchTransaction tx, Controller controller) {
		if (!getContainer().getRealm(tx.getRealmName()).isUpdateObservers())
			return;

		ObserverEvent observerEvent = new ObserverEvent();
		observerEvent.removed.addElement(Tags.CONTROLLER, rootElement);
		tx.getContainer().getRealm(tx.getRealmName()).getObserverHandler().notify(observerEvent);
	}

	private void notifyObserverRemove(String realm, Controller controller) {
		if (!getContainer().getRealm(realm).isUpdateObservers())
			return;

		try {
			runAsAgent(ctx -> {
				try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), true)) {
					Activity activity = tx.findElement(activityLoc, true);
					if (activity != null) {
						ObserverEvent observerEvent = new ObserverEvent();
						observerEvent.removed.addElement(Tags.CONTROLLER, activity);
						getContainer().getRealm(realm).getObserverHandler().notify(observerEvent);
					}
				}
			});
		} catch (Exception e) {
			logger.error("Failed to notify observers of removed controller " + activityLoc);
		}
	}

	private void notifyObserverRemove(String realm, Map<Locator, Controller> removed) {
		if (!getContainer().getRealm(realm).isUpdateObservers())
			return;

		try {
			runAsAgent(ctx -> {
				try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), true)) {
					ObserverEvent observerEvent = new ObserverEvent();

					for (Locator activityLoc : activityLocs) {
						Activity activity = tx.findElement(activityLoc, true);
						if (activity != null)
							observerEvent.removed.addElement(Tags.CONTROLLER, activity);
					}

					getContainer().getRealm(realm).getObserverHandler().notify(observerEvent);
				}
			});
		} catch (Exception e) {
			logger.error("Failed to notify observers of removed controllers " + activityLocs);
		}
	}

	@Override
	public DelayedExecutionTimer getDelayedExecutionTimer() {
		return this.delayedExecutionTimer;
	}
}
