package li.strolch.execution.policy;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.command.UpdateActivityCommand;
import li.strolch.execution.DurationExecutionTimer;
import li.strolch.model.Locator;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;

public class SimulationPolicy extends ExecutionPolicy {

	private Action action;

	public SimulationPolicy(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	public void execute(Action action) {

		this.action = action;

		String realmName = tx().getRealmName();
		Locator locator = action.getLocator();
		DurationExecutionTimer.getInstance().execute(realmName, getContainer(), locator, 2000L);

		action.setState(State.EXECUTION);

		UpdateActivityCommand command = new UpdateActivityCommand(getContainer(), tx());
		command.setActivity(action.getRootElement());
		command.doCommand();

		logger.info("Started execution of " + action.getLocator());
	}

	@Override
	public void executed(Action action) {

		action.setState(State.EXECUTED);

		UpdateActivityCommand command = new UpdateActivityCommand(getContainer(), tx());
		command.setActivity(action.getRootElement());
		command.doCommand();

		logger.info("Completed execution of " + action.getLocator());
	}

	@Override
	public void stop(Action action) {

		DurationExecutionTimer.getInstance().stop(action.getLocator());

		action.setState(State.STOPPED);

		UpdateActivityCommand command = new UpdateActivityCommand(getContainer(), tx());
		command.setActivity(action.getRootElement());
		command.doCommand();

		logger.info("Stopped execution of " + action.getLocator());
	}

	@Override
	public void undo() {
		stop(this.action);
	}
}
