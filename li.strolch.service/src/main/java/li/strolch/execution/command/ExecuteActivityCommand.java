package li.strolch.execution.command;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

public class ExecuteActivityCommand extends ExecutionCommand {

	private Activity activity;

	public ExecuteActivityCommand(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	public void setActivity(Activity activity) {
		this.activity = activity;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("activity can not be null!", this.activity);
	}

	@Override
	public void doCommand() {
		this.activity.accept(this);
	}

	@Override
	public void undo() {
		// can't undo execution
	}
}
