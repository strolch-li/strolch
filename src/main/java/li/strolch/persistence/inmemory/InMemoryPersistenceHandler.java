package li.strolch.persistence.inmemory;

import li.strolch.agent.api.StrolchComponent;
import li.strolch.agent.impl.ComponentContainerImpl;
import li.strolch.agent.impl.StrolchRealm;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;

public class InMemoryPersistenceHandler extends StrolchComponent implements PersistenceHandler {

	private OrderDao orderDao;
	private ResourceDao resourceDao;

	public InMemoryPersistenceHandler(ComponentContainerImpl container, String componentName) {
		super(container, componentName);
	}

	@Override
	public StrolchTransaction openTx(StrolchRealm realm) {
		return new InMemoryTransaction(realm, this);
	}

	public OrderDao getOrderDao() {
		if (this.orderDao == null)
			this.orderDao = new InMemoryOrderDao();
		return this.orderDao;
	}

	public ResourceDao getResourceDao() {
		if (this.resourceDao == null)
			this.resourceDao = new InMemoryResourceDao();
		return this.resourceDao;
	}
}
