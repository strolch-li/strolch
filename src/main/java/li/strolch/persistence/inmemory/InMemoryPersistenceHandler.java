package li.strolch.persistence.inmemory;

import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.agent.api.StrolchComponent;
import li.strolch.runtime.agent.impl.ComponentContainerImpl;

public class InMemoryPersistenceHandler extends StrolchComponent implements PersistenceHandler {

	private OrderDao orderDao;
	private ResourceDao resourceDao;

	public InMemoryPersistenceHandler(ComponentContainerImpl container, String componentName) {
		super(container, componentName);
	}

	@Override
	public StrolchTransaction openTx() {
		return openTx(StrolchConstants.DEFAULT_REALM);
	}

	@Override
	public StrolchTransaction openTx(String realm) {
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
