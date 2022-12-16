package li.strolch.execution.command;

import java.text.MessageFormat;

import li.strolch.exception.StrolchException;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

public class SetActionToClosedCommand extends BasePlanningAndExecutionCommand {

	private Action action;

	public SetActionToClosedCommand(StrolchTransaction tx) {
		super(tx);
	}

	public void setAction(Action action) {
		this.action = action;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("action can not be null", this.action);

		if (!this.action.getState().canSetToClosed()) {
			String msg = "Current state is {0} and can not be changed to {1} for action {2}";
			msg = MessageFormat.format(msg, this.action.getState(), State.CLOSED, this.action.getLocator());
			throw new StrolchException(msg);
		}
	}

	@Override
	public void doCommand() {
		if (this.action.getState() == State.CLOSED) {
			logger.warn("Action " + this.action.getLocator() + " is already in state CLOSED! Not changing.");
			return;
		}

		Activity rootElement = this.action.getRootElement();
		State currentState = rootElement.getState();

		this.action.setState(State.CLOSED);

		getConfirmationPolicy(this.action).toClosed(this.action);

		updateOrderState(tx(), rootElement, currentState, rootElement.getState());
	}
}
