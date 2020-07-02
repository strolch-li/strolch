package li.strolch.agent.impl;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_ACTIVITY_REF;

import java.util.List;

import li.strolch.agent.api.ActivityMap;
import li.strolch.model.activity.Activity;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.query.ActivityQuery;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.query.inmemory.InMemoryActivityQueryVisitor;
import li.strolch.runtime.query.inmemory.InMemoryQuery;

public class TransientActivityMap extends TransientElementMap<Activity> implements ActivityMap {

	@Override
	protected void assertIsRefParam(Parameter<?> refP) {
		ElementMapHelpers.assertIsRefParam(INTERPRETATION_ACTIVITY_REF, refP);
	}

	@Override
	public <U> List<U> doQuery(StrolchTransaction tx, ActivityQuery<U> activityQuery) {
		InMemoryActivityQueryVisitor visitor = new InMemoryActivityQueryVisitor();
		InMemoryQuery<Activity, U> query = visitor.visit(activityQuery);
		return query.doQuery(tx, this);
	}
}
