package li.strolch.agent.impl;

import li.strolch.agent.api.OrderMap;
import li.strolch.model.Order;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.StrolchTransaction;

public class CachedOrderMap extends CachedElementMap<Order> implements OrderMap {

	@Override
	protected OrderDao getDao(StrolchTransaction tx) {
		return tx.getPersistenceHandler().getOrderDao(tx);
	}
}
