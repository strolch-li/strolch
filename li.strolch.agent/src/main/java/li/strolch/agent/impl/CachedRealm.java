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
package li.strolch.agent.impl;

import java.text.MessageFormat;
import java.util.List;
import java.util.Set;

import li.strolch.agent.api.ActivityMap;
import li.strolch.agent.api.AuditTrail;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.ActivityDao;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class CachedRealm extends InternalStrolchRealm {

	private PersistenceHandler persistenceHandler;
	private CachedResourceMap resourceMap;
	private CachedOrderMap orderMap;
	private CachedActivityMap activityMap;
	private AuditTrail auditTrail;

	public CachedRealm(String realm) {
		super(realm);
	}

	@Override
	public DataStoreMode getMode() {
		return DataStoreMode.CACHED;
	}

	@Override
	public StrolchTransaction openTx(Certificate certificate, String action) {
		DBC.PRE.assertNotNull("Certificate must be set!", certificate); //$NON-NLS-1$
		return this.persistenceHandler.openTx(this, certificate, action);
	}

	@Override
	public StrolchTransaction openTx(Certificate certificate, Class<?> clazz) {
		DBC.PRE.assertNotNull("Certificate must be set!", certificate); //$NON-NLS-1$
		return this.persistenceHandler.openTx(this, certificate, clazz.getName());
	}

	@Override
	public ResourceMap getResourceMap() {
		return this.resourceMap;
	}

	@Override
	public OrderMap getOrderMap() {
		return this.orderMap;
	}

	@Override
	public ActivityMap getActivityMap() {
		return this.activityMap;
	}

	@Override
	public AuditTrail getAuditTrail() {
		return this.auditTrail;
	}

	@Override
	public void initialize(ComponentContainer container, ComponentConfiguration configuration) {
		super.initialize(container, configuration);

		this.persistenceHandler = container.getComponent(PersistenceHandler.class);
		this.resourceMap = new CachedResourceMap();
		this.orderMap = new CachedOrderMap();
		this.activityMap = new CachedActivityMap();

		if (isAuditTrailEnabled()) {
			this.auditTrail = new CachedAuditTrail();
			logger.info("Enabling AuditTrail for realm " + getRealm()); //$NON-NLS-1$
		} else {
			this.auditTrail = new NoStrategyAuditTrail();
			logger.info("AuditTrail is disabled for realm " + getRealm()); //$NON-NLS-1$
		}
	}

	@Override
	public void start(PrivilegeContext privilegeContext) {

		long start = System.nanoTime();
		int nrOfOrders = 0;
		int nrOfResources = 0;
		int nrOfActivities = 0;

		try (StrolchTransaction tx = openTx(privilegeContext.getCertificate(), DefaultRealmHandler.AGENT_BOOT)) {
			ResourceDao resourceDao = tx.getPersistenceHandler().getResourceDao(tx);
			Set<String> resourceTypes = resourceDao.queryTypes();
			for (String type : resourceTypes) {
				List<Resource> resources = resourceDao.queryAll(type);
				for (Resource resource : resources) {
					this.resourceMap.insert(resource);
					nrOfResources++;
				}
			}

			tx.commitOnClose();
		}

		try (StrolchTransaction tx = openTx(privilegeContext.getCertificate(), DefaultRealmHandler.AGENT_BOOT)) {
			OrderDao orderDao = tx.getPersistenceHandler().getOrderDao(tx);
			Set<String> orderTypes = orderDao.queryTypes();
			for (String type : orderTypes) {
				List<Order> orders = orderDao.queryAll(type);
				for (Order order : orders) {
					this.orderMap.insert(order);
					nrOfOrders++;
				}
			}

			tx.commitOnClose();
		}

		try (StrolchTransaction tx = openTx(privilegeContext.getCertificate(), DefaultRealmHandler.AGENT_BOOT)) {
			ActivityDao activityDao = tx.getPersistenceHandler().getActivityDao(tx);
			Set<String> activityTypes = activityDao.queryTypes();
			for (String type : activityTypes) {
				List<Activity> activities = activityDao.queryAll(type);
				for (Activity activity : activities) {
					this.activityMap.insert(activity);
					nrOfActivities++;
				}
			}

			tx.commitOnClose();
		}

		long duration = System.nanoTime() - start;
		String durationS = StringHelper.formatNanoDuration(duration);
		logger.info(MessageFormat.format("Loading Model from Database for realm {0} took {1}.", getRealm(), durationS)); //$NON-NLS-1$
		logger.info(MessageFormat.format("Loaded {0} Orders", nrOfOrders)); //$NON-NLS-1$
		logger.info(MessageFormat.format("Loaded {0} Resources", nrOfResources)); //$NON-NLS-1$
		logger.info(MessageFormat.format("Loaded {0} Activities", nrOfActivities)); //$NON-NLS-1$
	}

	@Override
	public void stop() {
		// 
	}

	@Override
	public void destroy() {
		// 
	}
}
