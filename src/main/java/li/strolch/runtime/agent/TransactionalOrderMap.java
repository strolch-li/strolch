package li.strolch.runtime.agent;

import li.strolch.model.Order;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.StrolchTransaction;

public class TransactionalOrderMap extends AbstractTransactionalElementMap<Order> implements OrderMap {

	public TransactionalOrderMap(String realm, PersistenceHandler persistenceHandler) {
		super(realm, persistenceHandler);
	}

	@Override
	protected OrderDao getDao(StrolchTransaction tx) {
		return tx.getOrderDao();
	}
}
