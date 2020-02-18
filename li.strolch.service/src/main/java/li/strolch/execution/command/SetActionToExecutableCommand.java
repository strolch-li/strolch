package li.strolch.execution.command;

import java.text.MessageFormat;

import li.strolch.exception.StrolchException;
import li.strolch.model.State;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;

public class SetActionToExecutableCommand extends ActionExecutionCommand {

	public SetActionToExecutableCommand(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public void validate() {
		super.validate();

		if (!this.action.getState().canSetToExecutable()) {
			String msg = "Current state is {0} and can not be changed to {1} for action {2}";
			msg = MessageFormat.format(msg, this.action.getState(), State.EXECUTABLE, this.action.getLocator());
			throw new StrolchException(msg);
		}
	}

	@Override
	public void doCommand() {
		if (this.action.getState() == State.EXECUTABLE) {
			logger.warn("Action " + this.action.getLocator() + " is already in state EXECUTABLE! Not changing.");
			return;
		}

		Activity rootElement = this.action.getRootElement();
		State currentState = rootElement.getState();

		this.action.setState(State.EXECUTABLE);

		getConfirmationPolicy(this.action).toExecutable(this.action);

		updateOrderState(tx(), rootElement, currentState, rootElement.getState());
	}
}
