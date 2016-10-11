package li.strolch.execution.policy;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.command.UpdateActivityCommand;
import li.strolch.execution.DelayedExecutionTimer;
import li.strolch.execution.ExecutionHandler;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicy;

public abstract class ExecutionPolicy extends StrolchPolicy {

	public ExecutionPolicy(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	public abstract void toExecution(Action action);

	public abstract void toExecuted(Action action);

	public abstract void toStopped(Action action);

	public abstract void toError(Action action);

	public void toWarning(Action action) {

		action.setState(State.WARNING);

		UpdateActivityCommand command = new UpdateActivityCommand(getContainer(), tx());
		command.setActivity(action.getRootElement());
		command.doCommand();

		logger.warn("Action " + action.getLocator() + " is now in WARNING!");
	}

	protected DelayedExecutionTimer getDelayedExecutionTimer() {
		return getComponent(ExecutionHandler.class).getDelayedExecutionTimer();
	}
}
