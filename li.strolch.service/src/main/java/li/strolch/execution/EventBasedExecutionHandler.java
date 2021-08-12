package li.strolch.execution;

import static java.util.Collections.emptySet;
import static li.strolch.model.StrolchModelConstants.*;
import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;
import static li.strolch.utils.collections.SynchronizedCollections.synchronizedMapOfMaps;

import java.util.*;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.ObserverEvent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.execution.command.ArchiveActivityCommand;
import li.strolch.execution.policy.ExecutionPolicy;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.*;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogMessageState;
import li.strolch.model.log.LogSeverity;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.utils.collections.MapOfMaps;

/**
 * The event based execution handler waits for events in that the {@link ExecutionPolicy} implementations must call the
 * relevant methods when the work is complete. Afterwards the next {@link Action} in the procedure is executed
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class EventBasedExecutionHandler extends ExecutionHandler {

	private Map<String, ExecutionHandlerState> statesByRealm;
	private MapOfMaps<String, Locator, Controller> controllers;

	private DelayedExecutionTimer delayedExecutionTimer;

	public EventBasedExecutionHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public Collection<Controller> getControllers(String realm) {
		Map<Locator, Controller> controllersByRealm = this.controllers.getMap(realm);
		if (controllersByRealm == null)
			return Collections.emptyList();
		return controllersByRealm.values();
	}

	@Override
	public Controller getController(String realm, Activity activity) {
		return this.controllers.getElement(realm, activity.getLocator());
	}

	@Override
	public Controller getController(String realm, Locator locator) {
		return this.controllers.getElement(realm, locator.trim(3));
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
		this.controllers = synchronizedMapOfMaps(new MapOfMaps<>(true));
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
	public void toExecution(String realm, Activity activity) {
		ExecutionHandlerState state = this.statesByRealm.getOrDefault(realm, ExecutionHandlerState.Running);
		if (state == ExecutionHandlerState.HaltNew)
			throw new IllegalStateException(
					"ExecutionHandler state is " + state + ", can not add activities for execution!");

		Controller controller = this.controllers.getElement(realm, activity.getLocator());
		if (controller == null) {
			controller = new Controller(realm, this, activity);
			this.controllers.addElement(realm, activity.getLocator(), controller);
			notifyObserverAdd(controller);
		}

		toExecution(controller);
	}

	@Override
	public void toExecution(String realm, Locator activityLoc) {
		ExecutionHandlerState state = this.statesByRealm.getOrDefault(realm, ExecutionHandlerState.Running);
		if (state == ExecutionHandlerState.HaltNew)
			throw new IllegalStateException(
					"ExecutionHandler state is " + state + ", can not add activities for execution!");

		Controller controller = this.controllers.getElement(realm, activityLoc);
		if (controller == null)
			throw new IllegalStateException("No controller registered for activity " + activityLoc);

		toExecution(controller);
	}

	@Override
	public void removeFromExecution(Controller controller) {
		if (this.controllers.removeElement(controller.getRealm(), controller.getLocator()) != null) {
			logger.info("Removed controller " + controller.getLocator() + " from execution.");
			getExecutor().submit(() -> notifyObserverRemove(controller));
		} else {
			logger.error(
					"Controller " + controller.getRealm() + " " + controller.getLocator() + " was already removed.");
		}
	}

	@Override
	public void removeFromExecution(String realm, Locator activityLoc) {
		Locator rootElemLoc = activityLoc.trim(3);
		Controller controller = this.controllers.removeElement(realm, rootElemLoc);
		if (controller != null)
			getExecutor().submit(() -> notifyObserverRemove(controller));
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

				// we only want to restart activities which were in execution
				State state = activity.getState();
				if (!state.inExecutionPhase())
					return;

				if (activity.isReadOnly())
					activity = activity.getClone(true);

				logger.info("Restarting Execution of " + activity.getLocator() + " on realm " + realmName);

				// in execution actions need to be in state STOPPED to restart
				activity.findActionsDeep(a -> a.getState().inExecutionPhase()).forEach(a -> {
					// but not if is in error:
					if (state != State.ERROR)
						a.setState(State.STOPPED);
				});

				tx.update(activity);

				// register for execution
				Controller controller = new Controller(realmName, this, activity);
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
			if (controllers != null)
				controllers.values().forEach(this::toExecution);
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
				Resource executionHandlerConfig = tx.getResourceBy(TYPE_CONFIGURATION,
						ExecutionHandler.class.getSimpleName());
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
			Resource executionHandlerConfig = tx.getResourceBy(TYPE_CONFIGURATION,
					ExecutionHandler.class.getSimpleName());
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

	private void toExecution(Controller controller) {

		String realm = controller.getRealm();
		ExecutionHandlerState state = this.statesByRealm.getOrDefault(realm, ExecutionHandlerState.Running);
		if (state == ExecutionHandlerState.Paused) {
			logger.warn("Ignoring execution of " + controller.getLocator() + " for paused realm " + realm);
			return;
		}

		getExecutor().execute(() -> {
			try {
				controller.execute();
			} catch (Exception e) {
				logger.error("Failed to set " + controller.getLocator() + " to execution", e);

				if (getContainer().hasComponent(OperationsLog.class)) {
					getComponent(OperationsLog.class).addMessage(
							new LogMessage(realm, SYSTEM_USER_AGENT, controller.getLocator(), LogSeverity.Exception,
									LogMessageState.Information, ResourceBundle.getBundle("strolch-service"),
									"execution.handler.failed.execution").withException(e).value("reason", e));
				}
			}
		});
	}

	@Override
	public void toExecuted(String realm, Locator locator) {
		getExecutor().execute(() -> {
			try {

				Controller controller = this.controllers.getElement(realm, locator.trim(3));
				if (controller != null)
					controller.toExecuted(locator);

			} catch (Exception e) {
				logger.error("Failed to set " + locator + " to executed due to " + e.getMessage(), e);

				if (getContainer().hasComponent(OperationsLog.class)) {
					getComponent(OperationsLog.class).addMessage(
							new LogMessage(realm, SYSTEM_USER_AGENT, locator, LogSeverity.Exception,
									LogMessageState.Information, ResourceBundle.getBundle("strolch-service"),
									"execution.handler.failed.executed").withException(e).value("reason", e));
				}
			}
		});
	}

	@Override
	public void toStopped(String realm, Locator locator) {
		getExecutor().execute(() -> {
			try {

				Controller controller = this.controllers.getElement(realm, locator.trim(3));
				if (controller != null)
					controller.toStopped(locator);

			} catch (Exception e) {
				logger.error("Failed to set " + locator + " to stopped due to " + e.getMessage(), e);

				if (getContainer().hasComponent(OperationsLog.class)) {
					getComponent(OperationsLog.class).addMessage(
							new LogMessage(realm, SYSTEM_USER_AGENT, locator, LogSeverity.Exception,
									LogMessageState.Information, ResourceBundle.getBundle("strolch-service"),
									"execution.handler.failed.stopped").withException(e).value("reason", e));
				}
			}
		});
	}

	@Override
	public void toError(String realm, Locator locator) {
		getExecutor().execute(() -> {
			try {

				Controller controller = this.controllers.getElement(realm, locator.trim(3));
				if (controller != null)
					controller.toError(locator);

			} catch (Exception e) {
				logger.error("Failed to set " + locator + " to error due to " + e.getMessage(), e);

				if (getContainer().hasComponent(OperationsLog.class)) {
					getComponent(OperationsLog.class).addMessage(
							new LogMessage(realm, SYSTEM_USER_AGENT, locator, LogSeverity.Exception,
									LogMessageState.Information, ResourceBundle.getBundle("strolch-service"),
									"execution.handler.failed.error").withException(e).value("reason", e));
				}
			}
		});
	}

	@Override
	public void toWarning(String realm, Locator locator) {
		getExecutor().execute(() -> {
			try {

				Controller controller = this.controllers.getElement(realm, locator.trim(3));
				if (controller != null)
					controller.toWarning(locator);

			} catch (Exception e) {
				logger.error("Failed to set " + locator + " to warning due to " + e.getMessage(), e);

				if (getContainer().hasComponent(OperationsLog.class)) {
					getComponent(OperationsLog.class).addMessage(
							new LogMessage(realm, SYSTEM_USER_AGENT, locator, LogSeverity.Exception,
									LogMessageState.Information, ResourceBundle.getBundle("strolch-service"),
									"execution.handler.failed.warning").withException(e).value("reason", e));
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
									LogMessageState.Information, ResourceBundle.getBundle("strolch-service"),
									"execution.handler.failed.archive").withException(e).value("reason", e));
				}
			}
		});
	}

	private void notifyObserverAdd(Controller controller) {
		StrolchRealm realm = getContainer().getRealm(controller.getRealm());
		if (!realm.isUpdateObservers())
			return;

		ObserverEvent observerEvent = new ObserverEvent();
		observerEvent.added.addElement(Tags.CONTROLLER, controller.getActivity());
		realm.getObserverHandler().notify(observerEvent);
	}

	private void notifyObserverRemove(Controller controller) {
		StrolchRealm realm = getContainer().getRealm(controller.getRealm());
		if (!realm.isUpdateObservers())
			return;

		ObserverEvent observerEvent = new ObserverEvent();
		observerEvent.removed.addElement(Tags.CONTROLLER, controller.getActivity());
		realm.getObserverHandler().notify(observerEvent);
	}

	private void notifyObserverRemove(String realmName, Map<Locator, Controller> removed) {
		StrolchRealm realm = getContainer().getRealm(realmName);
		if (!realm.isUpdateObservers())
			return;

		ObserverEvent observerEvent = new ObserverEvent();
		for (Controller controller : removed.values()) {
			observerEvent.removed.addElement(Tags.CONTROLLER, controller.getActivity());
		}
		realm.getObserverHandler().notify(observerEvent);
	}

	@Override
	public DelayedExecutionTimer getDelayedExecutionTimer() {
		return this.delayedExecutionTimer;
	}
}
