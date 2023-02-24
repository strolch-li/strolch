package li.strolch.execution.policy;

import static li.strolch.model.StrolchModelConstants.PolicyConstants.PARAM_DURATION;

import li.strolch.model.activity.Action;
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

	protected boolean delayToExecuted = true;

	public DurationExecution(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public void toExecution(Action action) {
		if (this.delayToExecuted) {
			super.toExecution(action);
			if (action.findObjectivesParam(PARAM_DURATION, true).isEmpty())
				toExecuted(action);
			else
				delayToExecuted(action);
		} else {
			super.toExecution(action);
		}
	}
}
