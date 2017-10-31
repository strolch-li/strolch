package li.strolch.execution.policy;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;

public class RemoveActivityArchival extends ActivityArchivalPolicy {

	public RemoveActivityArchival(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	public void archive(Activity activity) {

		if (!activity.getState().isExecuted() && !activity.getState().isClosed())
			throw new IllegalStateException("Can not archive non-executed " + activity.getLocator());

		tx().remove(activity);

		logger.info("Removing " + activity.getLocator());
	}
}
