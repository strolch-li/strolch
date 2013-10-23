package li.strolch.persistence.impl;

import li.strolch.model.Order;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.StrolchTransaction;

public class XmlOrderDao extends AbstractDao<Order> implements OrderDao {

	private static final String CLASS_TYPE = Order.class.getName();

	protected XmlOrderDao(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	protected String getClassType() {
		return CLASS_TYPE;
	}
}
