/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 */
package li.strolch.agent.impl;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.xml.StrolchElementListener;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;

public class StoreToDaoElementListener implements StrolchElementListener {

	private StrolchTransaction tx;
	private ResourceDao resourceDao;
	private OrderDao orderDao;

	public StoreToDaoElementListener(StrolchTransaction tx) {
		this.tx = tx;
		this.resourceDao = tx.getPersistenceHandler().getResourceDao(this.tx);
		this.orderDao = tx.getPersistenceHandler().getOrderDao(this.tx);
	}

	@Override
	public void notifyResource(Resource resource) {
		this.resourceDao.save(resource);
	}

	@Override
	public void notifyOrder(Order order) {
		this.orderDao.save(order);
	}
}