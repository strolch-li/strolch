package li.strolch.agent.impl;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_ORDER_REF;
import static li.strolch.model.StrolchModelConstants.UOM_NONE;

import java.text.MessageFormat;

import li.strolch.agent.api.OrderMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.Order;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.StrolchTransaction;

public class CachedOrderMap extends CachedElementMap<Order> implements OrderMap {

	@Override
	public Order getBy(StrolchTransaction tx, StringParameter refP) throws StrolchException {

		if (!refP.getInterpretation().equals(INTERPRETATION_ORDER_REF)) {
			String msg = "{0} is not an Order reference as its interpretation is not {1}"; //$NON-NLS-1$
			throw new StrolchException(MessageFormat.format(msg, refP.getLocator(), INTERPRETATION_ORDER_REF));
		}

		if (refP.getUom().equals(UOM_NONE)) {
			String msg = "{0} is not an Order reference as its UOM is not set to a type!"; //$NON-NLS-1$
			throw new StrolchException(MessageFormat.format(msg, refP.getLocator()));
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
