package li.strolch.execution.policy;

import li.strolch.agent.api.ComponentContainer;
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

	public DurationExecution(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	public void toExecution(Action action) {

		DurationParameter durationP = action
				.findParameter(PolicyConstants.BAG_OBJECTIVES, PolicyConstants.PARAM_DURATION, true);

		String realmName = tx().getRealmName();
		Locator locator = action.getLocator();
		logger.info("Executing action " + action.getLocator() + " has a duration of " + durationP.getValueAsString());
		getDelayedExecutionTimer().execute(realmName, getContainer(), locator, durationP.toMillis());

		super.toExecution(action);
	}
}
