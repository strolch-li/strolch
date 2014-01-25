package li.strolch.persistence.inmemory;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.agent.impl.StrolchRealm;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;

public class InMemoryPersistenceHandler extends StrolchComponent implements PersistenceHandler {

	private OrderDao orderDao;
	private ResourceDao resourceDao;

	public InMemoryPersistenceHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public StrolchTransaction openTx(StrolchRealm realm) {
		return new InMemoryTransaction(realm, this);
	}

	@Override
	public OrderDao getOrderDao(StrolchTransaction tx) {
		if (this.orderDao == null)
			this.orderDao = new InMemoryOrderDao();
		return this.orderDao;
	}

	@Override
	public ResourceDao getResourceDao(StrolchTransaction tx) {
		if (this.resourceDao == null)
			this.resourceDao = new InMemoryResourceDao();
		return this.resourceDao;
	}
}
