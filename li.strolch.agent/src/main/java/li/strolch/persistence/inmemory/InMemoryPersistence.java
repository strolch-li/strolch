/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package li.strolch.persistence.inmemory;

import java.util.HashMap;
import java.util.Map;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.persistence.api.ActivityDao;
import li.strolch.persistence.api.AuditDao;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;

public class InMemoryPersistence implements PersistenceHandler {

	private boolean versioningEnabled;
	private Map<String, DaoCache> daoCache;
	private ComponentContainer container;

	public InMemoryPersistence(ComponentContainer container, boolean versioningEnabled) {
		this.container = container;
		this.versioningEnabled = versioningEnabled;
		this.daoCache = new HashMap<>();
	}

	@Override
	public StrolchTransaction openTx(StrolchRealm realm, Certificate certificate, String action) {
		return new InMemoryTransaction(this.container, realm, certificate, action, this);
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
	public ActivityDao getActivityDao(StrolchTransaction tx) {
		DaoCache daoCache = getDaoCache(tx);
		return daoCache.getActivityDao();
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
			daoCache = new DaoCache(new InMemoryOrderDao(this.versioningEnabled),
					new InMemoryResourceDao(this.versioningEnabled), new InMemoryActivityDao(this.versioningEnabled),
					new InMemoryAuditDao());
			this.daoCache.put(tx.getRealmName(), daoCache);
		}
		return daoCache;
	}

	private class DaoCache {
		private OrderDao orderDao;
		private ResourceDao resourceDao;
		private ActivityDao activityDao;
		private AuditDao auditDao;

		public DaoCache(OrderDao orderDao, ResourceDao resourceDao, ActivityDao activityDao, AuditDao auditDao) {
			this.orderDao = orderDao;
			this.resourceDao = resourceDao;
			this.activityDao = activityDao;
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

		public ActivityDao getActivityDao() {
			return this.activityDao;
		}
	}
}
