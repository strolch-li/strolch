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
package li.strolch.persistence.xml;

import java.io.File;
import java.util.Properties;

import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.xmlpers.api.IoMode;
import ch.eitchnet.xmlpers.api.PersistenceConstants;
import ch.eitchnet.xmlpers.api.PersistenceManager;
import ch.eitchnet.xmlpers.api.PersistenceManagerLoader;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.Tags;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.ActivityDao;
import li.strolch.persistence.api.AuditDao;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.xml.model.AuditContextFactory;
import li.strolch.persistence.xml.model.OrderContextFactory;
import li.strolch.persistence.xml.model.ResourceContextFactory;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.StrolchConfigurationException;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlPersistenceHandler extends StrolchComponent implements PersistenceHandler {

	public static final String DB_STORE_PATH = "dbStore/"; //$NON-NLS-1$
	private PersistenceManager persistenceManager;

	public XmlPersistenceHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration componentConfiguration) throws Exception {

		File basePathF = componentConfiguration.getRuntimeConfiguration().getDataPath();
		File dbStorePathF = new File(basePathF, DB_STORE_PATH);
		if (!dbStorePathF.exists() && !dbStorePathF.mkdir()) {
			throw new StrolchConfigurationException("Could not create store path at " + dbStorePathF.getAbsolutePath());
		}

		Properties properties = new Properties();
		properties.setProperty(PersistenceConstants.PROP_VERBOSE, "true"); //$NON-NLS-1$
		properties.setProperty(PersistenceConstants.PROP_XML_IO_MOD, IoMode.DOM.name());
		properties.setProperty(PersistenceConstants.PROP_BASEPATH, dbStorePathF.getAbsolutePath());

		this.persistenceManager = PersistenceManagerLoader.load(properties);

		this.persistenceManager.getCtxFactory().registerPersistenceContextFactory(Resource.class, Tags.RESOURCE,
				new ResourceContextFactory());
		this.persistenceManager.getCtxFactory().registerPersistenceContextFactory(Order.class, Tags.ORDER,
				new OrderContextFactory());
		this.persistenceManager.getCtxFactory().registerPersistenceContextFactory(Audit.class, Tags.AUDIT,
				new AuditContextFactory());

		super.initialize(componentConfiguration);
	}

	@SuppressWarnings("resource")
	@Override
	public StrolchTransaction openTx(StrolchRealm realm, Certificate certificate, String action) {
		PersistenceTransaction tx = this.persistenceManager.openTx(realm.getRealm());
		return new XmlStrolchTransaction(getContainer().getPrivilegeHandler(), realm, certificate, action, tx, this);
	}

	@Override
	public OrderDao getOrderDao(StrolchTransaction tx) {
		return new XmlOrderDao(tx);
	}

	@Override
	public ResourceDao getResourceDao(StrolchTransaction tx) {
		return new XmlResourceDao(tx);
	}

	@Override
	public ActivityDao getActivityDao(StrolchTransaction tx) {
		return new XmlActivityDao(tx);
	}

	@Override
	public AuditDao getAuditDao(StrolchTransaction tx) {
		return new XmlAuditDao(tx);
	}

	@Override
	public void performDbInitialization() {
		throw new UnsupportedOperationException("Not yet implemented!");
	}
}
