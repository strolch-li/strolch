package li.strolch.execution.command;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.State;
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
		tx().lock(this.activity.getRootElement());
	}

	@Override
	public void doCommand() {
		Activity rootElement = this.activity.getRootElement();
		tx().lock(rootElement);

		State currentState = rootElement.getState();
		this.activity.accept(this);

		updateOrderState(tx(), rootElement, currentState, rootElement.getState());
	}

	@Override
	public void undo() {
		// can't undo execution
	}
}
