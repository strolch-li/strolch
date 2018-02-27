package li.strolch.agent.impl;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_RESOURCE_REF;

import java.util.List;

import li.strolch.agent.api.ResourceMap;
import li.strolch.model.Resource;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.query.ResourceQuery;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.query.inmemory.InMemoryQuery;
import li.strolch.runtime.query.inmemory.InMemoryResourceQueryVisitor;

public class TransientResourceMap extends TransientElementMap<Resource> implements ResourceMap {

	@Override
	protected void assertIsRefParam(Parameter<?> refP) {
		ElementMapHelpers.assertIsRefParam(INTERPRETATION_RESOURCE_REF, refP);
	}

	@Override
	public <U> List<U> doQuery(StrolchTransaction tx, ResourceQuery<U> resourceQuery) {
		InMemoryResourceQueryVisitor visitor = new InMemoryResourceQueryVisitor();
		InMemoryQuery<Resource, U> query = visitor.visit(resourceQuery);
		return query.doQuery(tx, this);
	}
}
