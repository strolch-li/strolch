package li.strolch.execution;

import java.util.Collection;
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
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.privilege.PrivilegedRunnable;

/**
 * <p>
 * The ExecutionHandler enables the automated execution of {@link Activity} and {@link Action} elements.
 * </p>
 *
 * <p>
 * To start the execution of an {@link Activity} add it to the {@link ExecutionHandler} by calling {@link
 * #toExecution(String, Activity)} or {@link #toExecution(String, Activity)}. Actual execution is asynchronously
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

	public ExecutionHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	public static final String PROP_RESTART_EXECUTION = "restartExecution";
	public static final String PROP_LOCK_RETRIES = "lockRetries";

	public static final String PARAM_STATE = "state";

	public StrolchTransaction openTx(String realm, Certificate cert, Class<?> action, boolean readOnly) {
		return super.openTx(realm, cert, action.getName(), readOnly);
	}

	public void runAsAgent(PrivilegedRunnable runnable) throws PrivilegeException, Exception {
		super.runAsAgent(runnable);
	}

	public ExecutorService getExecutor() {
		return getExecutorService("ExecutionHandler");
	}

	/**
	 * Returns the controllers for the given realm
	 *
	 * @param realm
	 * 		the realm for which to get the controller
	 *
	 * @return the controllers
	 */
	public abstract Collection<Controller> getControllers(String realm);

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
	 * Registers the given {@link Activity} for execution, and submits it for execution immediately in an asynchronous
	 * manner
	 *
	 * @param realm
	 * 		the realm where the {@link Activity} resides
	 * @param activity
	 * 		the the {@link Activity}
	 */
	public abstract void toExecution(String realm, Activity activity);

	/**
	 * Removes the given {@link Controller} from execution, so it is not executed further
	 *
	 * @param controller
	 * 		the controller to remove
	 */
	public abstract void removeFromExecution(Controller controller);

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
	 * Restarts all existing Activities which are not yet executed and already in state of execution
	 *
	 * @param ctx
	 * 		the privilege context
	 * @param realm
	 * 		the realm for which to restart activities
	 */
	public abstract void reloadActivitiesInExecution(PrivilegeContext ctx, String realm);

	/**
	 * Removes all currently registered {@link Activity Activities} from execution
	 *
	 * @param realm
	 * 		the realm for which to restart activities
	 */
	public abstract void clearAllCurrentExecutions(String realm);

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
	 * @param activity
	 * 		the {@link Activity}
	 */
	public abstract void archiveActivity(String realm, Activity activity);

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
	 * Completes the execution of the given {@link Action} with the given {@link Locator}
	 *
	 * @param realm
	 * 		the realm where the {@link Action} resides
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action}
	 */
	public abstract void toExecuted(String realm, Locator actionLoc);

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
	 * Sets the state of the {@link Action} with the given {@link Locator} to {@link State#WARNING}
	 *
	 * @param realm
	 * 		the realm where the {@link Action} resides
	 * @param actionLoc
	 * 		the {@link Locator} of the {@link Action}
	 */
	public abstract void toWarning(String realm, Locator actionLoc);

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
