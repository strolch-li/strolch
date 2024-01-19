package li.strolch.execution.command;

import li.strolch.execution.policy.ExecutionPolicy;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

public abstract class ActionExecutionCommand extends BasePlanningAndExecutionCommand {

	protected Action action;
	private ExecutionPolicy executionPolicy;

	public ActionExecutionCommand(StrolchTransaction tx) {
		super(tx);
	}

	public void setAction(Action action) {
		this.action = action;
	}

	public void setExecutionPolicy(ExecutionPolicy executionPolicy) {
		this.executionPolicy = executionPolicy;
	}

	protected ExecutionPolicy getExecutionPolicy(Action action) {
		if (this.executionPolicy == null) {
			this.executionPolicy = tx().getPolicy(action, ExecutionPolicy.class);
			this.executionPolicy.initialize(action);
		}
		return this.executionPolicy;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("action can not be null", this.action);
	}
}
