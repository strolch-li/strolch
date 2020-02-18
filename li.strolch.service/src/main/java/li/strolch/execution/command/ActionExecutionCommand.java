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
		if (this.executionPolicy != null)
			return this.executionPolicy;
		return tx().getPolicy(action.getPolicyDef(ExecutionPolicy.class));
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("action can not be null", this.action);
	}
}
