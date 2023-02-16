package li.strolch.execution;

import java.util.List;
import java.util.Set;
import java.util.concurrent.ExecutorService;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.execution.policy.DurationExecution;
import li.strolch.execution.policy.ExecutionPolicy;
import li.strolch.model.Locator;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.privilege.PrivilegedRunnable;
import li.strolch.runtime.privilege.PrivilegedRunnableWithResult;
import li.strolch.utils.helper.StringHelper;

/**
 * <p>
 * The ExecutionHandler enables the automated execution of {@link Activity} and {@link Action} elements.
 * </p>
 *
 * <p>
 * To start the execution of an {@link Activity} add it to the {@link ExecutionHandler} by calling
 * {@link #toExecution(String, Activity)} or {@link #toExecution(String, Activity)}. Actual execution is asynchronously
 * performed and the {@link ExecutionPolicy} of the resources of the {@link Action Actions} will perform the actual
 * execution.
 * </p>
 *
 * <p>
 * Execution of Actions is done either in series or in parallel, depending on the {@link TimeOrdering} on the relevant
 * {@link Activity}
 * </p>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class ExecutionHandler extends StrolchComponent {

	public static final String PROP_RESTART_EXECUTION = "restartExecution";
	public static final String PROP_LOCK_RETRIES = "lockRetries";

	public static final String PARAM_STATE = "state";

	private String defaultRealm;

	public ExecutionHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {
		Set<String> realmNames = getAgent().getContainer().getRealmNames();
		if (realmNames.size() == 1)
			this.defaultRealm = realmNames.iterator().next();
		super.initialize(configuration);
	}

	protected String getDefaultRealm() {
		if (StringHelper.isEmpty(this.defaultRealm))
			throw new IllegalStateException("No default realm defined!");
		return this.defaultRealm;
	}

	@Override
	public StrolchTransaction openTx(String realm, Certificate cert, Class<?> action, boolean readOnly) {
		return super.openTx(realm, cert, action.getName(), readOnly);
	}

	@Override
	public void runAsAgent(PrivilegedRunnable runnable) throws Exception {
		super.runAsAgent(runnable);
	}

	@Override
	public <T> T runAsAgentWithResult(PrivilegedRunnableWithResult<T> runnable) throws Exception {
		return super.runAsAgentWithResult(runnable);
	}

	public ExecutorService getExecutor() {
		return getExecutorService("ExecutionHandler");
	}

	/**
	 * Returns true if the given {@link Activity} is currently being controlled on the default realm
	 *
	 * @param activity
	 * 		the activity to check if it is being controlled
	 *
	 * @return true or false
	 *
	 * @throws IllegalStateException
	 * 		if the default realm is not set!
	 */
	public abstract boolean isControlling(Activity activity);

	/**
	 * Returns true if the given {@link Activity} is currently being controlled on the given realm
	 *
	 * @param realm
	 * 		the realm
	 * @param activity
	 * 		the activity to check if it is being controlled
	 *
	 * @return true or false
	 *
	 * @throws IllegalStateException
	 * 		if the default realm is not set!
	 */
	public abstract boolean isControlling(String realm, Activity activity);

	/**
	 * Returns true if the given {@link Activity} is currently being controlled on the default realm
	 *
	 * @param locator
	 * 		the locator of the activity to check if it is being controlled
	 *
	 * @return true or false
	 *
	 * @throws IllegalStateException
	 * 		if the default realm is not set!
	 */
	public abstract boolean isControlling(Locator locator);

	/**
	 * Returns true if the given {@link Activity} is currently being controlled on the given realm
	 *
	 * @param realm
	 * 		the realm
	 * @param locator
	 * 		the locator of the activity to check if it is being controlled
	 *
	 * @return true or false
	 */
	public abstract boolean isControlling(String realm, Locator locator);

	/**
	 * Returns the controllers for the default realm, if set
	 *
	 * @return the controllers
	 *
	 * @throws IllegalStateException
	 * 		if the default realm is not set!
	 */
	public abstract List<Controller> getControllers();

	/**
	 * Returns the controllers for the given realm
	 *
	 * @param realm
	 * 		the realm for which to get the controller
	 *
	 * @return the controllers
	 */
	public abstract List<Controller> getControllers(String realm);

	/**
	 * Returns the controller for the default realm and activity, null if it does not exist
	 *
	 * @param activity
	 * 		the activity for which to get the controller
	 *
	 * @return the controller, or null if it does not exist
	 *
	 * @throws IllegalStateException
	 * 		if the default realm is not set!
	 */
	public abstract Controller getController(Activity activity);

	/**
	 * Returns the controller for the given realm and activity, null if it does not exist
	 *
	 * @param realm
	 * 		the realm for which to get the controller
	 * @param activity
	 * 		the activity for which to get the controller
	 *
	 * @return the controller, or null if it does not exist
	 */
	public abstract Controller getController(String realm, Activity activity);

	/**
	 * Returns the controller for the default realm and activity, null if it does not exist
	 *
	 * @param locator
	 * 		the locator of the activity for which to get the controller
	 *
	 * @return the controller, or null if it does not exist
	 *
	 * @throws IllegalStateException
	 * 		if the default realm is not set!
	 */
	public abstract Controller getController(Locator locator);

	/**
	 * Returns the controller for the given realm and activity, null if it does not exist
	 *
	 * @param realm
	 * 		the realm for which to get the controller
	 * @param locator
	 * 		the locator of the activity for which to get the controller
	 *
	 * @return the controller, or null if it does not exist
	 */
	public abstract Controller getController(String realm, Locator locator);

	/**
	 * Registers the given {@link Activity} for execution on the default realm. Execution is started when the concrete
	 * implementation deems necessary
	 *
	 * @param activity
	 * 		the {@link Activity}
	 *
	 * @throws IllegalStateException
	 * 		if the default realm is not set!
	 */
	public abstract void addForExecution(Activity activity);

	/**
	 * Registers the given {@link Activity} for execution. Execution is started when the concrete implementation deems
	 * necessary
	 *
	 * @param realm
	 * 		the realm where the {@link Activity} resides
	 * @param activity
	 * 		the {@link Activity}
	 */
	public abstract void addForExecution(String realm, Activity activity);

	/**
	 * Registers the given {@link Activity} for execution on the default realm, and submits it for execution immediately
	 * in an asynchronous manner
	 *
	 * @param activity
	 * 		the {@link Activity}
	 *
	 * @throws IllegalStateException
	 * 		if the default realm is not set!
	 */
	public abstract void toExecution(Activity activity);

	/**
	 * Registers the given {@link Activity} for execution, and submits it for execution immediately in an asynchronous
	 * manner
	 *
	 * @param realm
	 * 		the realm where the {@link Activity} resides
	 * @param activity
	 * 		the {@link Activity}
	 */
	public abstract void toExecution(String realm, Activity activity);

	/**
	 * Triggers the execution of the given {@link Activity}'s {@link Locator} on the default realm. If the
	 * {@link Controller} is available, then it is triggered
	 *
	 * @param activityLoc
	 * 		the {@link Locator} for the activity to execute
	 *
	 * @throws IllegalStateException
	 * 		if the default realm is not set!
	 */
	public abstract void toExecution(Locator activityLoc);

	/**
	 * Triggers the execution of the given {@link Activity}'s {@link Locator}. If the {@link Controller} is available,
	 * then it is triggered
	 *
	 * @param realm
	 * 		the realm where the {@link Activity} resides
	 * @param activityLoc
	 * 		the {@link Locator} for the activity to execute
	 */
	public abstract void toExecution(String realm, Locator activityLoc);

	/**
	 * Removes the given {@link Controller} from execution, so it is not executed further
	 *
	 * @param controller
	 * 		the controller to remove
	 */
	public abstract void removeFromExecution(Controller controller);

	/**
	 * Removes the given {@link Locator} for an {@link Activity} from execution from the default realm, so it is not
	 * executed further
	 *
	 * @param activityLoc
	 * 		the {@link Locator} of the {@link Activity}
	 *
	 * @throws IllegalStateException
	 * 		if the default realm is not set!
	 */
	public abstract void removeFromExecution(Locator activityLoc);

	/**
	 * Removes the given {@link Locator} for an {@link Activity} from execution, so it is not executed further
	 *
	 * @param realm
	 * 		the realm where the {@link Activity} resides
	 * @param activityLoc
	 * 		the {@link Locator} of the {@link Activity}
	 */
	public abstract void removeFromExecution(String realm, Locator activityLoc);

	/**
	 * Restarts all existing Activities on the given realm, which are not yet executed and already in state of
	 * execution
	 *
	 * @param ctx
	 * 		the privilege context
	 *
	 * @throws IllegalStateException
	 * 		if the default realm is not set!
	 */
	public abstract void reloadActivitiesInExecution(PrivilegeContext ctx);

	/**
	 * Restarts all existing Activities which are not yet executed and already in state of execution
	 *
	 * @param ctx
	 * 		the privilege context
	 * @param realm
	 * 		the realm for which to restart activities
	 */
	public abstract void reloadActivitiesInExecution(PrivilegeContext ctx, String realm);

	/**
	 * Removes all currently registered {@link Activity Activities} from execution from the default realm
	 *
	 * @throws IllegalStateException
	 * 		if the default realm is not set!
	 */
	public abstract void clearAllCurrentExecutions();

	/**
	 * Removes all currently registered {@link Activity Activities} from execution
	 *
	 * @param realm
	 * 		the realm for which to restart activities
	 */
	public abstract void clearAllCurrentExecutions(String realm);

	/**
	 * Triggers execution for all registered activities in the default realm
	 *
	 * @throws IllegalStateException
	 * 		if the default realm is not set!
	 */
	public abstract void triggerExecution();

	/**
	 * Triggers execution for all registered activities in the given realm
	 *
	 * @param realm
	 * 		the realm to trigger execution for
	 */
	public abstract void triggerExecution(String realm);

	/**
	 * Get the sate of the execution handler
	 *
	 * @param realm
	 * 		the realm for which to get the state
	 *
	 * @return the state of the execution handler
	 */
	public abstract ExecutionHandlerState getState(String realm);

	/**
	 * Set the state for the given realm
	 *
	 * @param cert
	 * 		certificate to use
	 * @param realm
	 * 		the realm to halt execution for
	 * @param state
	 * 		the state to set
	 */
	public abstract void setState(Certificate cert, String realm, ExecutionHandlerState state);

	/**
	 * Archives the given {@link Activity}
	 *
	 * @param realm
	 * 		the realm where the activity resides
	 * @param activityLoc
	 * 		the {@link Locator} of the {@link Activity} to archive
	 */
	public abstract void archiveActivity(String realm, Locator activityLoc);

	/**
	 * Returns the {@link Set} of {@link Locator Locators} of {@link Activity Activities} which are registered for
	 * execution for the default realm
	 *
	 * @return a set of locators
	 *
	 * @throws IllegalStateException
	 * 		if the default realm is not set!
	 */
	public abstract Set<Locator> getActiveActivitiesLocator();

	/**
	 * Returns the {@link Set} of {@link Locator Locators} of {@link Activity Activities} which are registered for
	 * execution for the given realm
	 *
	 * @param realm
	 * 		the realm for which to return the registered activities
	 *
	 * @return a set of locators
	 */
	public abstract Set<Locator> getActiveActivitiesLocator(String realm);

	/**
	 * <p>
	 * Returns the {@link DelayedExecutionTimer}
	 * </p>
	 *
	 * <p>
	 * The {@link DelayedExecutionTimer} allows to delay the {@link Controller#toExecuted(Locator)} call by a given
	 * time. See the {@link DurationExecution} policy
	 * </p>
	 *
	 * @return the {@link DelayedExecutionTimer}
	 */
	public abstract DelayedExecutionTimer getDelayedExecutionTimer();

	/**
	 * Completes the execution of the given {@link Action} with the given {@link Locator} on the default realm
	 *
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action}
	 *
	 * @throws IllegalStateException
	 * 		if the default realm is not set!
	 */
	public abstract void toExecuted(Locator actionLoc);

	/**
	 * Completes the execution of the given {@link Action} with the given {@link Locator}
	 *
	 * @param realm
	 * 		the realm where the {@link Action} resides
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action}
	 */
	public abstract void toExecuted(String realm, Locator actionLoc);

	/**
	 * Sets the state of the {@link Action} with the given {@link Locator} to {@link State#STOPPED} on the default
	 * realm
	 *
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action}
	 *
	 * @throws IllegalStateException
	 * 		if the default realm is not set!
	 */
	public abstract void toStopped(Locator actionLoc);

	/**
	 * Sets the state of the {@link Action} with the given {@link Locator} to {@link State#STOPPED}
	 *
	 * @param realm
	 * 		the realm where the {@link Action} resides
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action}
	 */
	public abstract void toStopped(String realm, Locator actionLoc);

	/**
	 * Sets the state of the {@link Action} with the given {@link Locator} to {@link State#WARNING} on the default
	 * realm
	 *
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action}
	 *
	 * @throws IllegalStateException
	 * 		if the default realm is not set!
	 */
	public abstract void toWarning(Locator actionLoc);

	/**
	 * Sets the state of the {@link Action} with the given {@link Locator} to {@link State#WARNING}
	 *
	 * @param realm
	 * 		the realm where the {@link Action} resides
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action}
	 */
	public abstract void toWarning(String realm, Locator actionLoc);

	/**
	 * Sets the state of the {@link Action} with the given {@link Locator} to {@link State#ERROR} on the default realm
	 *
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action}
	 *
	 * @throws IllegalStateException
	 * 		if the default realm is not set!
	 */
	public abstract void toError(Locator actionLoc);

	/**
	 * Sets the state of the {@link Action} with the given {@link Locator} to {@link State#ERROR}
	 *
	 * @param realm
	 * 		the realm where the {@link Action} resides
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action}
	 */
	public abstract void toError(String realm, Locator actionLoc);

}
