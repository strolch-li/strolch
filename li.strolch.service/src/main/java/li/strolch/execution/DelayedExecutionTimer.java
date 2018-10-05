package li.strolch.execution;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.execution.policy.ExecutionPolicy;
import li.strolch.model.Locator;
import li.strolch.model.activity.Action;

/**
 * A decoupling of {@link ExecutionPolicy ExecutionPolicies} so that the execution can be performed after a certain
 * period. The {@link #execute(String, ComponentContainer, Locator, long)} will complete the execution of this task
 * after the given duration
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface DelayedExecutionTimer {

	/**
	 * Completes the execution of the given {@link Action} {@link Locator} after the given duration in milliseconds
	 *
	 * @param realm
	 * 		the realm from which to retrieve the {@link Action}
	 * @param container
	 * 		reference to the container
	 * @param actionLocator
	 * 		the {@link Action}'s {@link Locator}
	 * @param duration
	 * 		the duration in milliseconds before calling the {@link ExecutionPolicy#toExecuted(Action)}
	 */
	void execute(String realm, ComponentContainer container, Locator actionLocator, long duration);

	/**
	 * Cancels any delayed execution for the given {@link Locator}
	 *
	 * @param locator
	 * 		the {@link Action}'s {@link Locator}
	 */
	void cancel(Locator locator);

	/**
	 * Destroys this timer by canceling on tasks and stopping the timer
	 */
	void destroy();
}