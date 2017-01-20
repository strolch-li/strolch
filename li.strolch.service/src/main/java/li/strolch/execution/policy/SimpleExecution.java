package li.strolch.execution.policy;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.command.UpdateActivityCommand;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;

/**
 * <p>
 * Simple Execution Policy which sets the state of the action depending on the method called.
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class SimpleExecution extends ExecutionPolicy {

	public SimpleExecution(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	public void toExecution(Action action) {

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

		getDelayedExecutionTimer().cancel(action.getLocator());

		action.setState(State.STOPPED);

		UpdateActivityCommand command = new UpdateActivityCommand(getContainer(), tx());
		command.setActivity(action.getRootElement());
		command.doCommand();

		logger.warn("Action " + action.getLocator() + " is now STOPPED!");
	}

	@Override
	public void toError(Action action) {

		getDelayedExecutionTimer().cancel(action.getLocator());

		action.setState(State.ERROR);

		UpdateActivityCommand command = new UpdateActivityCommand(getContainer(), tx());
		command.setActivity(action.getRootElement());
		command.doCommand();

		logger.error("Action " + action.getLocator() + " is now in ERROR!");
	}

	@Override
	public void undo() {
		logger.error("Can not undo execution policy " + getClass());
	}
}
