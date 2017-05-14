package li.strolch.execution.command;

import java.text.MessageFormat;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.exception.StrolchException;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

public class SetActionToStoppedCommand extends ExecutionCommand {

	private Action action;

	public SetActionToStoppedCommand(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	public void setAction(Action action) {
		this.action = action;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("action can not be null", this.action);

		if (!this.action.getState().canSetToStopped()) {
			String msg = "State {0} and canot be changed to {1} for action {2}";
			msg = MessageFormat.format(msg, this.action.getState(), State.STOPPED, this.action.getLocator());
			throw new StrolchException(msg);
		}
	}

	@Override
	public void doCommand() {
		tx().lock(this.action.getRootElement());

		getExecutionPolicy(this.action).toStopped(this.action);
		getConfirmationPolicy(this.action).toStopped(this.action);
	}

	@Override
	public void undo() {
		// can not undo
	}
}
