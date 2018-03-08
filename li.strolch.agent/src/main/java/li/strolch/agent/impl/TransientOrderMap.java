package li.strolch.agent.impl;

import static li.strolch.runtime.StrolchConstants.INTERPRETATION_ORDER_REF;

import java.util.Date;
import java.util.List;

import li.strolch.agent.api.OrderMap;
import li.strolch.model.Order;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.query.OrderQuery;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.query.inmemory.InMemoryOrderQueryVisitor;
import li.strolch.runtime.query.inmemory.InMemoryQuery;

public class TransientOrderMap extends TransientElementMap<Order> implements OrderMap {

	@Override
	public Order getTemplate(StrolchTransaction tx, String type, boolean assertExists) {
		Order template = super.getTemplate(tx, type, assertExists);
		template.setDate(new Date());
		return template;
	}

	@Override
	protected void assertIsRefParam(Parameter<?> refP) {
		ElementMapHelpers.assertIsRefParam(INTERPRETATION_ORDER_REF, refP);
	}

	@Override
	public <U> List<U> doQuery(StrolchTransaction tx, OrderQuery<U> orderQuery) {
		InMemoryOrderQueryVisitor visitor = new InMemoryOrderQueryVisitor();
		InMemoryQuery<Order, U> query = visitor.visit(orderQuery);
		return query.doQuery(tx, this);
	}
}
