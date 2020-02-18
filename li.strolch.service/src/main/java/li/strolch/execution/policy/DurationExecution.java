package li.strolch.execution.policy;

import li.strolch.model.Locator;
import li.strolch.model.activity.Action;
import li.strolch.model.parameter.DurationParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants.PolicyConstants;

/**
 * <p>
 * Simple Execution Policy which starts the execution immediately, i.e. set state to in execution and completes after
 * the {@link Action Action's} duration has passed.
 * </p>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DurationExecution extends SimpleExecution {

	protected Locator actionLoc;

	public DurationExecution(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public void toExecution(Action action) {

		DurationParameter durationP = action
				.findParameter(PolicyConstants.BAG_OBJECTIVES, PolicyConstants.PARAM_DURATION, true);

		String realmName = tx().getRealmName();
		this.actionLoc = action.getLocator();
		logger.info("Executing action " + actionLoc + " has a duration of " + durationP.getValueAsString());
		getDelayedExecutionTimer().execute(realmName, getContainer(), this.actionLoc, durationP.toMillis());

		super.toExecution(action);
	}

	@Override
	protected void handleStopped() {
		getDelayedExecutionTimer().cancel(this.actionLoc);
	}
}
