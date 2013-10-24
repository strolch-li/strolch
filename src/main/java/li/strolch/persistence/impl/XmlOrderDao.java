package li.strolch.persistence.impl;

import li.strolch.model.Order;
import li.strolch.model.Tags;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.StrolchTransaction;

public class XmlOrderDao extends AbstractDao<Order> implements OrderDao {

	protected XmlOrderDao(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	protected String getClassType() {
		return Tags.ORDER;
	}
}
