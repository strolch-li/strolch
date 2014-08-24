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

import li.strolch.agent.api.AuditTrail;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.configuration.ComponentConfiguration;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.utils.dbc.DBC;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class TransactionalRealm extends StrolchRealm {

	private ResourceMap resourceMap;
	private OrderMap orderMap;
	private AuditTrail auditTrail;
	private PersistenceHandler persistenceHandler;

	public TransactionalRealm(String realm) {
		super(realm);
	}

	@Override
	public DataStoreMode getMode() {
		return DataStoreMode.TRANSACTIONAL;
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
	public AuditTrail getAuditTrail() {
		return this.auditTrail;
	}

	@Override
	public void initialize(ComponentContainer container, ComponentConfiguration configuration) {
		super.initialize(container, configuration);
		this.resourceMap = new TransactionalResourceMap();
		this.orderMap = new TransactionalOrderMap();

		String enableAuditKey = DefaultRealmHandler.makeRealmKey(getRealm(),
				DefaultRealmHandler.PROP_ENABLE_AUDIT_TRAIL);

		if (configuration.getBoolean(enableAuditKey, Boolean.FALSE)) {
			this.auditTrail = new TransactionalAuditTrail();
			logger.info("Enabling AuditTrail for realm " + getRealm()); //$NON-NLS-1$
		} else {
			this.auditTrail = new NoStrategyAuditTrail();
			logger.info("AuditTrail is disabled for realm " + getRealm()); //$NON-NLS-1$
		}

		this.persistenceHandler = container.getComponent(PersistenceHandler.class);
	}

	@Override
	public void start(PrivilegeContext privilegeContext) {

		long start = System.nanoTime();
		int nrOfOrders = 0;
		int nrOfResources = 0;

		try (StrolchTransaction tx = openTx(privilegeContext.getCertificate(), DefaultRealmHandler.AGENT_BOOT)) {
			nrOfOrders = this.orderMap.getAllKeys(tx).size();
		}

		try (StrolchTransaction tx = openTx(privilegeContext.getCertificate(), DefaultRealmHandler.AGENT_BOOT)) {
			nrOfResources = this.resourceMap.getAllKeys(tx).size();
		}

		long duration = System.nanoTime() - start;
		String durationS = StringHelper.formatNanoDuration(duration);
		logger.info(MessageFormat.format(
				"Initialized Transactional Maps for realm {0} took {1}.", getRealm(), durationS)); //$NON-NLS-1$
		logger.info(MessageFormat.format("There are {0} Orders", nrOfOrders)); //$NON-NLS-1$
		logger.info(MessageFormat.format("There are {0} Resources", nrOfResources)); //$NON-NLS-1$
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
