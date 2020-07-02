package li.strolch.execution.policy;

import static li.strolch.model.StrolchModelConstants.PolicyConstants.BAG_OBJECTIVES;
import static li.strolch.model.StrolchModelConstants.PolicyConstants.PARAM_DURATION;

import li.strolch.model.activity.Action;
import li.strolch.model.parameter.DurationParameter;
import li.strolch.persistence.api.StrolchTransaction;

/**
 * <p>
 * Simple Execution Policy which starts the execution immediately, i.e. set state to in execution and completes after
 * the {@link Action Action's} duration has passed.
 * </p>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DurationExecution extends SimpleExecution {

	public DurationExecution(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public void toExecution(Action action) {

		DurationParameter durationP = action.findParameter(BAG_OBJECTIVES, PARAM_DURATION, true);
		delayToExecutedBy(durationP.getDuration());

		super.toExecution(action);
	}
}
