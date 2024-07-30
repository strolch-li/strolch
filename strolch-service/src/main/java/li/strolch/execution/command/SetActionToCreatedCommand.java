package li.strolch.execution.command;

import java.text.MessageFormat;

import li.strolch.exception.StrolchException;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

public class SetActionToCreatedCommand extends BasePlanningAndExecutionCommand {

	private Action action;

	public SetActionToCreatedCommand(StrolchTransaction tx) {
		super(tx);
	}

	public void setAction(Action action) {
		this.action = action;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("action can not be null", this.action);

		if (!this.action.getState().canSetToCreated()) {
			String msg = "Current state is {0} and can not be changed to {1} for action {2}";
			msg = MessageFormat.format(msg, this.action.getState(), State.CREATED, this.action.getLocator());
			throw new StrolchException(msg);
		}
	}

	@Override
	public void doCommand() {
		if (this.action.getState() == State.CREATED) {
			logger.warn("Action {} is already in state CREATED! Not changing.", this.action.getLocator());
			return;
		}

		Activity rootElement = this.action.getRootElement();
		State currentState = rootElement.getState();

		this.action.setState(State.CREATED);

		getConfirmationPolicy(this.action).toCreated(this.action);

		updateOrderState(tx(), rootElement, currentState, rootElement.getState());
	}
}
