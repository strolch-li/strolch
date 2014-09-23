package li.strolch.persistence.inmemory;

import java.util.HashMap;
import java.util.Map;

import li.strolch.agent.api.StrolchRealm;
import li.strolch.persistence.api.AuditDao;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.privilege.PrivilegeHandler;
import ch.eitchnet.privilege.model.Certificate;

public class InMemoryPersistence implements PersistenceHandler {

	private Map<String, DaoCache> daoCache;
	private PrivilegeHandler privilegeHandler;

	public InMemoryPersistence(PrivilegeHandler privilegeHandler) {
		this.privilegeHandler = privilegeHandler;
		this.daoCache = new HashMap<>();
	}

	@Override
	public StrolchTransaction openTx(StrolchRealm realm, Certificate certificate, String action) {
		return new InMemoryTransaction(this.privilegeHandler, realm, certificate, action, this);
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

	@Override
	public AuditDao getAuditDao(StrolchTransaction tx) {
		DaoCache daoCache = getDaoCache(tx);
		return daoCache.getAuditDao();
	}

	@Override
	public void performDbInitialization() {
		// no-op
	}

	private synchronized DaoCache getDaoCache(StrolchTransaction tx) {
		DaoCache daoCache = this.daoCache.get(tx.getRealmName());
		if (daoCache == null) {
			daoCache = new DaoCache(new InMemoryOrderDao(), new InMemoryResourceDao(), new InMemoryAuditDao());
			this.daoCache.put(tx.getRealmName(), daoCache);
		}
		return daoCache;
	}

	private class DaoCache {
		private OrderDao orderDao;
		private ResourceDao resourceDao;
		private AuditDao auditDao;

		public DaoCache(OrderDao orderDao, ResourceDao resourceDao, AuditDao auditDao) {
			this.orderDao = orderDao;
			this.resourceDao = resourceDao;
			this.auditDao = auditDao;
		}

		public OrderDao getOrderDao() {
			return this.orderDao;
		}

		public ResourceDao getResourceDao() {
			return this.resourceDao;
		}

		public AuditDao getAuditDao() {
			return this.auditDao;
		}
	}
}
