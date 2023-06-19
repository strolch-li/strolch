package li.strolch.execution.policy;

import static li.strolch.model.StrolchModelConstants.PolicyConstants.PARAM_DURATION;

import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;

import java.util.concurrent.TimeUnit;

/**
 * <p>
 * Simple Execution Policy which starts the execution immediately, i.e. set state to in execution and completes after
 * the {@link Action Action's} duration has passed.
 * </p>
 *
 * <p>Sub classes can by pass the delaying of completion by directly calling
 * {@link #setActionStateWithValueChange(Action, State, double)}. This is useful when calling any of the
 * {@link #delayRandom(long, TimeUnit, Runnable)} methods</p>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DurationExecution extends SimpleExecution {

	public DurationExecution(StrolchTransaction tx) {
		super(tx);
	}

	/**
	 * Delays completion of the given action.
	 *
	 * <p>Sub classes can by pass the delaying of completion by directly calling
	 * {@link #setActionStateWithValueChange(Action, State, double)}</p>
	 *
	 * @param action the action to start execution for
	 */
	@Override
	public void toExecution(Action action) {
		super.toExecution(action);
		if (action.findObjectivesParam(PARAM_DURATION, true).isEmpty())
			toExecuted(action);
		else
			delayToExecuted(action);
	}
}
