package li.strolch.planning;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.Order;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicy;

public abstract class ActivityCreationPolicy extends StrolchPolicy {

	public ActivityCreationPolicy(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	public abstract Activity create(Order order);

	@Override
	public void undo() {
		// nothing to do
	}
}
