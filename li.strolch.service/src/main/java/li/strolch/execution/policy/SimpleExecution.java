package li.strolch.execution.policy;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.command.UpdateActivityCommand;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;

/**
 * <p>
 * Simple Execution Policy which sets the state of the action depending on the
 * method called.
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
		setActionState(action, State.EXECUTION);
	}

	@Override
	public void toExecuted(Action action) {
		setActionState(action, State.EXECUTED);
	}

	@Override
	public void toStopped(Action action) {
		getDelayedExecutionTimer().cancel(action.getLocator());
		setActionState(action, State.STOPPED);
	}

	@Override
	public void toError(Action action) {
		getDelayedExecutionTimer().cancel(action.getLocator());
		setActionState(action, State.ERROR);
	}

	protected void setActionState(Action action, State state) {

		action.setState(state);

		UpdateActivityCommand command = new UpdateActivityCommand(getContainer(), tx());
		command.setActivity(action.getRootElement());
		command.doCommand();

		String msg = "Action " + action.getLocator() + " is now in state " + state;
		if (state == State.ERROR)
			logger.error(msg);
		else if (state == State.STOPPED)
			logger.warn(msg);
		else
			logger.info(msg);
	}

	@Override
	public void undo() {
		logger.error("Can not undo execution policy " + getClass());
	}
}
