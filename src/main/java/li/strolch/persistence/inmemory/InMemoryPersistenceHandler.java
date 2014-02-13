package li.strolch.persistence.inmemory;

import java.util.HashMap;
import java.util.Map;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.agent.impl.StrolchRealm;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.configuration.ComponentConfiguration;

public class InMemoryPersistenceHandler extends StrolchComponent implements PersistenceHandler {

	private Map<String, DaoCache> daoCache;

	public InMemoryPersistenceHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public StrolchTransaction openTx(StrolchRealm realm) {
		return new InMemoryTransaction(realm, this);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) {
		super.initialize(configuration);
		this.daoCache = new HashMap<>();
	}

	@Override
	public OrderDao getOrderDao(StrolchTransaction tx) {
		DaoCache daoCache = getDaoCache(tx);
		return daoCache.getOrderDao();
	}

	@Override
	public ResourceDao getResourceDao(StrolchTransaction tx) {
		DaoCache daoCache = getDaoCache(tx);
		return daoCache.getResourceDao();
	}

	private synchronized DaoCache getDaoCache(StrolchTransaction tx) {
		DaoCache daoCache = this.daoCache.get(tx.getRealmName());
		if (daoCache == null) {
			daoCache = new DaoCache(new InMemoryOrderDao(), new InMemoryResourceDao());
			this.daoCache.put(tx.getRealmName(), daoCache);
		}
		return daoCache;
	}

	private class DaoCache {
		private OrderDao orderDao;
		private ResourceDao resourceDao;

		public DaoCache(OrderDao orderDao, ResourceDao resourceDao) {
			this.orderDao = orderDao;
			this.resourceDao = resourceDao;
		}

		public OrderDao getOrderDao() {
			return this.orderDao;
		}

		public ResourceDao getResourceDao() {
			return this.resourceDao;
		}
	}
}
