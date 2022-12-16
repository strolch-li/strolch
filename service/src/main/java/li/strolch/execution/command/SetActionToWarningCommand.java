package li.strolch.execution.command;

import java.text.MessageFormat;

import li.strolch.exception.StrolchException;
import li.strolch.model.State;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;

public class SetActionToWarningCommand extends ActionExecutionCommand {

	public SetActionToWarningCommand(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public void validate() {
		super.validate();

		if (!this.action.getState().canSetToWarning()) {
			String msg = "Current state is {0} and can not be changed to {1} for action {2}";
			msg = MessageFormat.format(msg, this.action.getState(), State.WARNING, this.action.getLocator());
			throw new StrolchException(msg);
		}
	}

	@Override
	public void doCommand() {
		if (this.action.getState() == State.WARNING) {
			logger.warn("Action " + this.action.getLocator() + " is already in state WARNING! Not changing.");
			return;
		}

		Activity rootElement = this.action.getRootElement();
		State currentState = rootElement.getState();

		getExecutionPolicy(this.action).toWarning(this.action);
		getConfirmationPolicy(this.action).toWarning(this.action);

		updateOrderState(tx(), rootElement, currentState, rootElement.getState());
	}
}
