package li.strolch.execution.policy;

import li.strolch.agent.api.ComponentContainer;
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
}
