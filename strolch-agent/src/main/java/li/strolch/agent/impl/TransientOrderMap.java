package li.strolch.agent.impl;

import li.strolch.agent.api.OrderMap;
import li.strolch.model.Order;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchTransaction;

import java.time.ZonedDateTime;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_ORDER_REF;

public class TransientOrderMap extends TransientElementMap<Order> implements OrderMap {

	@Override
	public Order getTemplate(StrolchTransaction tx, String type, boolean assertExists) {
		Order template = super.getTemplate(tx, type, assertExists);
		template.setDate(ZonedDateTime.now());
		return template;
	}

	@Override
	protected void assertIsRefParam(Parameter<?> refP) {
		ElementMapHelpers.assertIsRefParam(INTERPRETATION_ORDER_REF, refP);
	}
}
