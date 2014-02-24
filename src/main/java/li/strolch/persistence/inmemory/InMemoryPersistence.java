package li.strolch.persistence.inmemory;

import java.util.HashMap;
import java.util.Map;

import li.strolch.agent.api.StrolchRealm;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;

public class InMemoryPersistence implements PersistenceHandler {

	private Map<String, DaoCache> daoCache;

	public InMemoryPersistence() {
		this.daoCache = new HashMap<>();
	}

	@Override
	public StrolchTransaction openTx(StrolchRealm realm) {
		return new InMemoryTransaction(realm, this);
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
