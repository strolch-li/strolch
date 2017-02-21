package li.strolch.execution.command;

import java.text.MessageFormat;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.exception.StrolchException;
import li.strolch.execution.policy.ExecutionPolicy;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

public class SetActionToWarningCommand extends ExecutionCommand {

	private Action action;

	public SetActionToWarningCommand(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	public void setAction(Action action) {
		this.action = action;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("action can not be null", this.action);

		if (!this.action.getState().canSetToWarning()) {
			String msg = "State {0} and canot be changed to {1} for action {2}";
			msg = MessageFormat.format(msg, this.action.getState(), State.WARNING, this.action.getLocator());
			throw new StrolchException(msg);
		}
	}

	@Override
	public void doCommand() {
		tx().lock(this.action.getRootElement());
		ExecutionPolicy executionPolicy = getExecutionPolicy(this.action);
		executionPolicy.toWarning(this.action);
	}

	@Override
	public void undo() {
		// can not undo
	}
}
