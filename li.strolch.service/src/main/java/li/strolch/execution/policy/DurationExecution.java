package li.strolch.execution.policy;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.command.UpdateActivityCommand;
import li.strolch.execution.DurationExecutionTimer;
import li.strolch.model.Locator;
import li.strolch.model.State;
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
public class DurationExecution extends ExecutionPolicy {

	public DurationExecution(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	public void toExecution(Action action) {

		DurationParameter durationP = action.findParameter(PolicyConstants.BAG_OBJECTIVES,
				PolicyConstants.PARAM_DURATION, true);

		String realmName = tx().getRealmName();
		Locator locator = action.getLocator();
		logger.warn("Executing action " + action.getLocator() + " has a duration of " + durationP.getValueAsString());
		DurationExecutionTimer.getInstance().execute(realmName, getContainer(), locator, durationP.getValue());

		action.setState(State.EXECUTION);

		UpdateActivityCommand command = new UpdateActivityCommand(getContainer(), tx());
		command.setActivity(action.getRootElement());
		command.doCommand();

		logger.info("Action " + action.getLocator() + " is now in EXECUTION!");
	}

	@Override
	public void toExecuted(Action action) {

		action.setState(State.EXECUTED);

		UpdateActivityCommand command = new UpdateActivityCommand(getContainer(), tx());
		command.setActivity(action.getRootElement());
		command.doCommand();

		logger.info("Action " + action.getLocator() + " is now EXECUTED!");
	}

	@Override
	public void toStopped(Action action) {

		DurationExecutionTimer.getInstance().cancel(action.getLocator());

		action.setState(State.STOPPED);

		UpdateActivityCommand command = new UpdateActivityCommand(getContainer(), tx());
		command.setActivity(action.getRootElement());
		command.doCommand();

		logger.warn("Action " + action.getLocator() + " is now STOPPED!");
	}

	@Override
	public void toError(Action action) {

		DurationExecutionTimer.getInstance().cancel(action.getLocator());

		action.setState(State.ERROR);

		UpdateActivityCommand command = new UpdateActivityCommand(getContainer(), tx());
		command.setActivity(action.getRootElement());
		command.doCommand();

		logger.error("Action " + action.getLocator() + " is now in ERROR!");
	}

	@Override
	public void undo() {
		logger.error("Can not undo an " + getClass());
	}
}
