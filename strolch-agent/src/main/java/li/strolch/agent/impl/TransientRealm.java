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

import li.strolch.agent.api.*;
import li.strolch.model.ModelStatistics;
import li.strolch.model.xml.XmlModelSaxFileReader;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.StrolchConfigurationException;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;

import java.io.File;
import java.text.MessageFormat;

import static li.strolch.agent.impl.DefaultRealmHandler.PREFIX_DATA_STORE_FILE;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class TransientRealm extends InternalStrolchRealm {

	private ResourceMap resourceMap;
	private OrderMap orderMap;
	private ActivityMap activityMap;
	private AuditTrail auditTrail;

	private File modelFile;

	public TransientRealm(String realm) {
		super(realm);
	}

	@Override
	public DataStoreMode getMode() {
		return DataStoreMode.TRANSIENT;
	}

	@Override
	public StrolchTransaction openTx(Certificate certificate, String action, boolean readOnly) {
		DBC.PRE.assertNotNull("Certificate must be set!", certificate);
		return new TransientTransaction(this.container, this, certificate, action, readOnly);
	}

	@Override
	public StrolchTransaction openTx(Certificate certificate, Class<?> clazz, boolean readOnly) {
		DBC.PRE.assertNotNull("Certificate must be set!", certificate);
		return new TransientTransaction(this.container, this, certificate, clazz.getName(), readOnly);
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

		String key = StrolchConstants.makeRealmKey(getRealm(), PREFIX_DATA_STORE_FILE);
		if (!configuration.hasProperty(key)) {
			String msg = "There is no data store file for realm {0}. Set a property with key {1}";
			msg = MessageFormat.format(msg, getRealm(), key);
			throw new StrolchConfigurationException(msg);
		}

		this.modelFile = configuration.getDataFile(key, null, configuration.getRuntimeConfiguration(), true);

		this.resourceMap = new TransientResourceMap();
		this.orderMap = new TransientOrderMap();
		this.activityMap = new TransientActivityMap();

		if (isAuditTrailEnabled())
			this.auditTrail = new TransientAuditTrail();
		else
			this.auditTrail = new NoStrategyAuditTrail();
	}

	@Override
	public void start(PrivilegeContext privilegeContext) {
		super.start(privilegeContext);

		ModelStatistics statistics;
		try (StrolchTransaction tx = openTx(privilegeContext.getCertificate(), "strolch_boot", false)) {
			InMemoryElementListener elementListener = new InMemoryElementListener(tx);

			// explicitly deny updating, so that we can detect XML files with duplicates
			elementListener.setUpdateResources(false);
			elementListener.setUpdateOrders(false);
			elementListener.setUpdateActivities(false);
			elementListener.setFailOnUpdate(true);

			XmlModelSaxFileReader handler = new XmlModelSaxFileReader(elementListener, this.modelFile, true);
			handler.parseFile();
			statistics = handler.getStatistics();
			tx.commitOnClose();
		}

		String durationS = StringHelper.formatNanoDuration(statistics.durationNanos);
		logger.info("Loaded XML Model file {} for realm {} took {}.", this.modelFile.getName(), getRealm(), durationS);
		logger.info("Loaded {} Orders", statistics.nrOfOrders);
		logger.info("Loaded {} Resources", statistics.nrOfResources);
		logger.info("Loaded {} Activities", statistics.nrOfActivities);
	}

	@Override
	public void destroy() {
		// 
	}
}
