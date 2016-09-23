package li.strolch.service.execution;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicy;

public abstract class ExecutionPolicy extends StrolchPolicy {

	public ExecutionPolicy(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	public abstract void execute(Action action);

	public abstract void executed(Action action);

	public abstract void stop(Action action);
}
