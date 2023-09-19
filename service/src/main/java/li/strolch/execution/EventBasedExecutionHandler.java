package li.strolch.execution;

import static java.util.Collections.emptyMap;
import static java.util.Collections.emptySet;
import static li.strolch.model.StrolchModelConstants.*;
import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;
import static li.strolch.utils.collections.SynchronizedCollections.synchronizedMapOfMaps;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;

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
import li.strolch.utils.collections.MapOfMaps;

/**
 * The event based execution handler waits for events in that the {@link ExecutionPolicy} implementations must call the
 * relevant methods when the work is complete. Afterwards the next {@link Action} in the procedure is executed
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class EventBasedExecutionHandler extends ExecutionHandler {

	protected final MapOfMaps<String, Locator, Controller> controllers;
	protected Map<String, ExecutionHandlerState> statesByRealm;
	protected DelayedExecutionTimer delayedExecutionTimer;

	public EventBasedExecutionHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
		this.controllers = synchronizedMapOfMaps(new MapOfMaps<>(true));
	}

	private static Locator trimLocator(Locator locator) {
		return locator.trim(3);
	}

	@Override
	public boolean isControlling(Activity activity) {
		return this.controllers.containsElement(getDefaultRealm(), activity.getLocator());
	}

	@Override
	public boolean isControlling(String realm, Activity activity) {
		return this.controllers.containsElement(realm, activity.getLocator());
	}

	@Override
	public boolean isControlling(Locator locator) {
		return this.controllers.containsElement(getDefaultRealm(), trimLocator(locator));
	}

	public boolean isControlling(String realm, Locator locator) {
		return this.controllers.containsElement(realm, trimLocator(locator));
	}

	@Override
	public List<Controller> getControllers() {
		return getControllers(getDefaultRealm());
	}

	@Override
	public List<Controller> getControllers(String realm) {
		Map<Locator, Controller> controllersByRealm = this.controllers.getMap(realm);
		if (controllersByRealm == null)
			return new ArrayList<>();
		return new ArrayList<>(controllersByRealm.values());
	}

	@Override
	public Controller getController(Activity activity) {
		return getController(getDefaultRealm(), activity.getLocator());
	}

	@Override
	public Controller getController(String realm, Activity activity) {
		return this.controllers.getElement(realm, activity.getLocator());
	}

	@Override
	public Controller getController(Locator locator) {
		return getController(getDefaultRealm(), locator);
	}

	@Override
	public Controller getController(String realm, Locator locator) {
		return this.controllers.getElement(realm, trimLocator(locator));
	}

	@Override
	public Set<Locator> getActiveActivitiesLocator() {
		return getActiveActivitiesLocator(getDefaultRealm());
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

	protected Controller newController(String realm, Activity activity) {
		return new Controller(realm, this, activity.ensureModifiable());
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

		// first stop and clear all existing controllers
		synchronized (this.controllers) {
			new HashSet<>(this.controllers.keySet()).forEach(this::stopControllers);
		}

		if (this.delayedExecutionTimer != null) {
			this.delayedExecutionTimer.destroy();
			this.delayedExecutionTimer = null;
		}

		super.stop();
	}

	protected void stopControllers(String realm) {
		logger.info("Stopping controllers for realm " + realm + "...");
		synchronized (this.controllers) {
			Map<Locator, Controller> map = this.controllers.getMap(realm);
			if (map == null) {
				logger.info("No controllers for realm " + realm);
				return;
			}

			map.values().forEach(controller -> {
				logger.info("Stopping controller " + controller);
				controller.stopExecutions();
			});

			this.controllers.removeMap(realm);
		}
	}

	@Override
	public void addForExecution(Activity activity) {
		addForExecution(getDefaultRealm(), activity);
	}

	@Override
	public void addForExecution(String realm, Activity activity) {
		ExecutionHandlerState state = this.statesByRealm.getOrDefault(realm, ExecutionHandlerState.Running);
		if (state == ExecutionHandlerState.HaltNew)
			throw new IllegalStateException(
					"ExecutionHandler state is " + state + ", can not add activities for execution!");

		if (this.controllers.containsElement(realm, activity.getLocator()))
			throw new IllegalStateException(activity.getLocator() + " is already registered for execution!");

		logger.info("Added " + activity.getLocator() + " @ " + realm);
		Controller controller = newController(realm, activity);
		this.controllers.addElement(realm, activity.getLocator(), controller);
		notifyObserverAdd(controller);

		triggerExecution(realm);
	}

	@Override
	public void toExecution(Activity activity) {
		toExecution(getDefaultRealm(), activity);
	}

	@Override
	public void toExecution(String realm, Activity activity) {
		ExecutionHandlerState state = this.statesByRealm.getOrDefault(realm, ExecutionHandlerState.Running);
		if (state == ExecutionHandlerState.HaltNew)
			throw new IllegalStateException(
					"ExecutionHandler state is " + state + ", can not add activities for execution!");

		Controller controller = this.controllers.getElement(realm, activity.getLocator());
		if (controller == null) {
			controller = newController(realm, activity);
			this.controllers.addElement(realm, activity.getLocator(), controller);
			notifyObserverAdd(controller);
		}

		toExecution(controller);
	}

	@Override
	public void toExecution(Locator activityLoc) {
		toExecution(getDefaultRealm(), activityLoc);
	}

	@Override
	public void toExecution(String realm, Locator activityLoc) {
		ExecutionHandlerState state = this.statesByRealm.getOrDefault(realm, ExecutionHandlerState.Running);
		if (state == ExecutionHandlerState.HaltNew)
			throw new IllegalStateException(
					"ExecutionHandler state is " + state + ", can not add activities for execution!");

		Locator trimmedLocator = trimLocator(activityLoc);
		Controller controller = this.controllers.getElement(realm, trimmedLocator);
		if (controller == null)
			throw new IllegalStateException("No controller registered for activity " + trimmedLocator);

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
	public void removeFromExecution(Locator activityLoc) {
		removeFromExecution(getDefaultRealm(), activityLoc);
	}

	@Override
	public void removeFromExecution(String realm, Locator activityLoc) {
		Controller controller = this.controllers.removeElement(realm, trimLocator(activityLoc));
		if (controller != null)
			getExecutor().submit(() -> notifyObserverRemove(controller));
	}

	@Override
	public void clearAllCurrentExecutions() {
		clearAllCurrentExecutions(getDefaultRealm());
	}

	@Override
	public void clearAllCurrentExecutions(String realm) {
		Map<Locator, Controller> removed = this.controllers.removeMap(realm);
		getExecutor().submit(() -> notifyObserverRemove(realm, removed));
	}

	protected void restartActivityExecution(PrivilegeContext ctx) {
		getContainer().getRealmNames().forEach(realmName -> reloadActivitiesInExecution(ctx, realmName));
	}

	@Override
	public void reloadActivitiesInExecution(PrivilegeContext ctx) {
		reloadActivitiesInExecution(ctx, getDefaultRealm());
	}

	@Override
	public void reloadActivitiesInExecution(PrivilegeContext ctx, String realmName) {
		try (StrolchTransaction tx = openTx(realmName, ctx.getCertificate(), false)) {

			// first stop and clear all existing controllers
			stopControllers(realmName);

			// iterate all activities
			tx.streamActivities().forEach(activity -> {

				// we only want to restart activities which were in execution
				State state = activity.getState();
				if (!state.inExecutionPlanningPhase())
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
				Controller controller = newController(realmName, activity);
				this.controllers.addElement(realmName, activity.getLocator(), controller);
			});

			// commit changes to state
			tx.commitOnClose();
		}

		// trigger execution of the registered activities
		logger.info("Triggering execution for realm " + realmName + " after reloading activities...");
		triggerExecution(realmName);
	}

	protected Stream<Controller> controllerStream(String realm) {
		return this.controllers.getMapOrDefault(realm, emptyMap()).values().stream();
	}

	@Override
	public void triggerExecution() {
		triggerExecution(getDefaultRealm());
	}

	@Override
	public void triggerExecution(String realm) {
		ExecutionHandlerState state = this.statesByRealm.getOrDefault(realm, ExecutionHandlerState.Running);
		if (state == ExecutionHandlerState.Paused) {
			logger.warn("Ignoring trigger for paused realm " + realm);
			return;
		}

		logger.info("Triggering execution for all controllers on realm " + realm + "...");
		getExecutor().execute(() -> {
			synchronized (this.controllers) {
				controllerStream(realm).forEach(this::toExecution);
			}
		});
	}

	protected void toExecution(Controller controller) {

		String realm = controller.getRealm();
		ExecutionHandlerState state = this.statesByRealm.getOrDefault(realm, ExecutionHandlerState.Running);
		if (state == ExecutionHandlerState.Paused) {
			logger.warn("Ignoring execution of " + controller.getLocator() + " for paused realm " + realm);
			return;
		}

		logger.info("Added toExecution task for " + controller.getLocator() + " @ " + realm);
		getExecutor().execute(() -> {
			try {

				// execute the controller
				boolean trigger = controller.execute();

				if (trigger) {
					logger.info("Triggering of controllers for realm " + realm + " after executing " + controller);
					triggerExecution(realm);
				}

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
	public void toExecuted(Locator actionLoc) {
		toExecuted(getDefaultRealm(), actionLoc);
	}

	@Override
	public void toExecuted(String realm, Locator locator) {
		logger.info("Added toExecuted task for " + locator + " @ " + realm);
		getExecutor().execute(() -> {
			try {

				Controller controller = this.controllers.getElement(realm, trimLocator(locator));
				if (controller != null)
					controller.toExecuted(locator);

				triggerExecution(realm);

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
	public void toStopped(Locator actionLoc) {
		toStopped(getDefaultRealm(), actionLoc);
	}

	@Override
	public void toStopped(String realm, Locator locator) {
		logger.warn("Added toStopped task for " + locator + " @ " + realm);
		getExecutor().execute(() -> {
			try {

				Controller controller = this.controllers.getElement(realm, trimLocator(locator));
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
	public void toError(Locator actionLoc) {
		toError(getDefaultRealm(), actionLoc);
	}

	@Override
	public void toError(String realm, Locator locator) {
		logger.error("Added toError task for " + locator + " @ " + realm);
		getExecutor().execute(() -> {
			try {

				Controller controller = this.controllers.getElement(realm, trimLocator(locator));
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
	public void toWarning(Locator actionLoc) {
		toWarning(getDefaultRealm(), actionLoc);
	}

	@Override
	public void toWarning(String realm, Locator locator) {
		logger.warn("Added toWarning task for " + locator + " @ " + realm);
		getExecutor().execute(() -> {
			try {

				Controller controller = this.controllers.getElement(realm, trimLocator(locator));
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
	public void archiveActivity(Locator activityLoc) {
		archiveActivity(getDefaultRealm(), activityLoc);
	}

	@Override
	public void archiveActivity(String realm, Locator activityLoc) {
		logger.info("Added archiveActivity task for " + activityLoc + " @ " + realm);
		Locator trimmedLocator = trimLocator(activityLoc);
		getExecutor().execute(() -> {
			try {
				runAsAgent(ctx -> {
					try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), ArchiveActivityCommand.class,
							false)) {
						ArchiveActivityCommand command = new ArchiveActivityCommand(tx);
						command.setActivityLoc(trimmedLocator);
						tx.addCommand(command);
						tx.commitOnClose();
					}
				});
			} catch (Exception e) {
				logger.error("Failed to archive " + trimmedLocator + " due to " + e.getMessage(), e);

				if (getContainer().hasComponent(OperationsLog.class)) {
					getComponent(OperationsLog.class).addMessage(
							new LogMessage(realm, SYSTEM_USER_AGENT, trimmedLocator, LogSeverity.Exception,
									LogMessageState.Information, ResourceBundle.getBundle("strolch-service"),
									"execution.handler.failed.archive").withException(e).value("reason", e));
				}
			}
		});

		triggerExecution(realm);
	}

	private void notifyObserverAdd(Controller controller) {
		StrolchRealm realm = getContainer().getRealm(controller.getRealm());
		if (!realm.isUpdateObservers())
			return;

		ObserverEvent observerEvent = new ObserverEvent();
		observerEvent.added.addElement(Tags.CONTROLLER, controller.getActivity().ensureReadOnly());
		realm.getObserverHandler().notify(observerEvent);
	}

	private void notifyObserverRemove(Controller controller) {
		StrolchRealm realm = getContainer().getRealm(controller.getRealm());
		if (!realm.isUpdateObservers())
			return;

		ObserverEvent observerEvent = new ObserverEvent();
		observerEvent.removed.addElement(Tags.CONTROLLER, controller.getActivity().ensureReadOnly());
		realm.getObserverHandler().notify(observerEvent);
	}

	private void notifyObserverRemove(String realmName, Map<Locator, Controller> removed) {
		StrolchRealm realm = getContainer().getRealm(realmName);
		if (!realm.isUpdateObservers())
			return;

		ObserverEvent observerEvent = new ObserverEvent();
		for (Controller controller : removed.values()) {
			observerEvent.removed.addElement(Tags.CONTROLLER, controller.getActivity().ensureReadOnly());
		}
		realm.getObserverHandler().notify(observerEvent);
	}

	@Override
	public DelayedExecutionTimer getDelayedExecutionTimer() {
		return this.delayedExecutionTimer;
	}

	@Override
	public ExecutionHandlerState getExecutionState() {
		return getExecutionState(getDefaultRealm());
	}

	@Override
	public ExecutionHandlerState getExecutionState(String realm) {
		return this.statesByRealm.getOrDefault(realm, ExecutionHandlerState.Running);
	}

	@Override
	public void getExecutionState(Certificate cert, ExecutionHandlerState state) {
		getExecutionState(cert, getDefaultRealm(), state);
	}

	@Override
	public void getExecutionState(Certificate cert, String realm, ExecutionHandlerState state) {
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

	private void evaluateStateByRealm() throws Exception {
		this.statesByRealm = new ConcurrentHashMap<>();

		runAsAgent(ctx -> getContainer().getRealmNames().forEach(realm -> {
			try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), false)) {

				Resource config = tx.getResourceBy(TYPE_CONFIGURATION, ExecutionHandler.class.getSimpleName());
				if (config == null) {
					this.statesByRealm.put(realm, ExecutionHandlerState.Running);
				} else {
					String currentState = config.getString(PARAM_STATE);
					ExecutionHandlerState state = ExecutionHandlerState.Running;
					try {
						if (!currentState.isEmpty())
							state = ExecutionHandlerState.valueOf(currentState);
					} catch (Exception e) {
						config.setString(PARAM_STATE, ExecutionHandlerState.Running);
						tx.update(config);
						tx.commitOnClose();
						logger.error("Failed to read unhandled state " + currentState, e);
					}

					this.statesByRealm.put(realm, state);
				}
			}
		}));
	}
}
