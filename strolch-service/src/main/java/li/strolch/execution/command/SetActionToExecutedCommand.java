package li.strolch.execution.command;

import java.text.MessageFormat;

import li.strolch.exception.StrolchException;
import li.strolch.model.State;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;

public class SetActionToExecutedCommand extends ActionExecutionCommand {

	public SetActionToExecutedCommand(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public void validate() {
		super.validate();

		if (!this.action.getState().canSetToExecuted()) {
			String msg = "Current state is {0} can not be changed to {1} for action {2}";
			msg = MessageFormat.format(msg, this.action.getState(), State.EXECUTED, this.action.getLocator());
			throw new StrolchException(msg);
		}
	}

	@Override
	public void doCommand() {
		if (this.action.getState() == State.EXECUTED) {
			logger.warn("Action {} is already in state EXECUTED! Not changing.", this.action.getLocator());
			return;
		}

		Activity rootElement = this.action.getRootElement();
		State currentState = rootElement.getState();

		getExecutionPolicy(this.action).toExecuted(this.action);
		getConfirmationPolicy(this.action).toExecuted(this.action);

		updateOrderState(tx(), rootElement, currentState, rootElement.getState());
	}
}
