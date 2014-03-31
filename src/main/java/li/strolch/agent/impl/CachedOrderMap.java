package li.strolch.agent.impl;

import static li.strolch.runtime.StrolchConstants.INTERPRETATION_ORDER_REF;
import li.strolch.agent.api.OrderMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.Order;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.StrolchTransaction;

public class CachedOrderMap extends CachedElementMap<Order> implements OrderMap {

	@Override
	public Order getBy(StrolchTransaction tx, StringParameter refP) throws StrolchException {

		if (!refP.getInterpretation().equals(INTERPRETATION_ORDER_REF)) {
			throw new StrolchException(refP.getLocator() + " is not an Order reference as its interpretation is not "
					+ INTERPRETATION_ORDER_REF);
		}

		if (refP.getUom().equals(Parameter.UOM_NONE)) {
			throw new StrolchException(refP.getLocator()
					+ " is not an Order reference as its UOM is not set to a type!");
		}

		String type = refP.getUom();
		String id = refP.getValue();

		return getBy(tx, type, id);
	}

	@Override
	protected OrderDao getDao(StrolchTransaction tx) {
		return tx.getPersistenceHandler().getOrderDao(tx);
	}
}
