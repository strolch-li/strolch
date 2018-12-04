package li.strolch.execution.command;

import java.text.MessageFormat;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.exception.StrolchException;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

public class SetActionToErrorCommand extends ExecutionCommand {

	private Action action;

	public SetActionToErrorCommand(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	public void setAction(Action action) {
		this.action = action;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("action can not be null", this.action);

		tx().lock(this.action.getRootElement());
		tx().lock(getResourceLocator(this.action));

		if (!this.action.getState().canSetToError()) {
			String msg = "Current state is {0} and can not be changed to {1} for action {2}";
			msg = MessageFormat.format(msg, this.action.getState(), State.ERROR, this.action.getLocator());
			throw new StrolchException(msg);
		}
	}

	@Override
	public void doCommand() {
		Activity rootElement = this.action.getRootElement();
		tx().lock(rootElement);
		tx().lock(getResourceLocator(this.action));

		if (this.action.getState() == State.ERROR) {
			logger.warn("Action " + this.action.getLocator() + " is already in ERROR! Not changing.");
			return;
		}

		State currentState = rootElement.getState();

		getExecutionPolicy(this.action).toError(this.action);
		getConfirmationPolicy(this.action).toError(this.action);

		updateOrderState(rootElement, currentState, rootElement.getState());
	}

	@Override
	public void undo() {
		// can not undo
	}
}
