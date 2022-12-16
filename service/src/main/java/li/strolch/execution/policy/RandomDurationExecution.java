package li.strolch.execution.policy;

import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;

/**
 * <p>
 * Simple Execution Policy which starts the execution immediately, i.e. set state to in execution and completes after
 * the {@link Action Action's} duration has passed, including a random min/max factor being applied to the duration
 * </p>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RandomDurationExecution extends SimpleExecution {

	public RandomDurationExecution(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public void toExecution(Action action) {
		delayToExecutedByRandom(action, 0.5, 2);
		super.toExecution(action);
	}
}
