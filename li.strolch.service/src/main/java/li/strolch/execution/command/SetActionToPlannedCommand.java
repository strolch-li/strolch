package li.strolch.execution.command;

import java.text.MessageFormat;

import li.strolch.exception.StrolchException;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

public class SetActionToPlannedCommand extends BasePlanningAndExecutionCommand {

	private Action action;

	public SetActionToPlannedCommand(StrolchTransaction tx) {
		super(tx);
	}

	public void setAction(Action action) {
		this.action = action;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("action can not be null", this.action);

		if (!this.action.getState().canSetToPlanned()) {
			String msg = "Current state is {0} and can not be changed to {1} for action {2}";
			msg = MessageFormat.format(msg, this.action.getState(), State.PLANNED, this.action.getLocator());
			throw new StrolchException(msg);
		}
	}

	@Override
	public void doCommand() {
		if (this.action.getState() == State.PLANNED) {
			logger.warn("Action " + this.action.getLocator() + " is already in state PLANNED! Not changing.");
			return;
		}

		Activity rootElement = this.action.getRootElement();
		State currentState = rootElement.getState();

		State currentActionState = this.action.getState();
		getPlanningPolicy(this.action).plan(this.action);
		if (currentActionState != this.action.getState() && this.action.isResourceDefined())
			getConfirmationPolicy(this.action).doConfirmation(this.action);

		updateOrderState(tx(), rootElement, currentState, rootElement.getState());
	}
}
