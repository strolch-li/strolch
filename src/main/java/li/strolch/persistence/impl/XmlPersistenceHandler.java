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
package li.strolch.persistence.impl;

import java.io.File;
import java.util.Properties;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.Tags;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchPersistenceHandler;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.impl.model.OrderContextFactory;
import li.strolch.persistence.impl.model.ResourceContextFactory;
import li.strolch.runtime.component.ComponentContainer;
import li.strolch.runtime.component.StrolchComponent;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.observer.ObserverHandler;
import ch.eitchnet.xmlpers.api.IoMode;
import ch.eitchnet.xmlpers.api.PersistenceConstants;
import ch.eitchnet.xmlpers.api.PersistenceManager;
import ch.eitchnet.xmlpers.api.PersistenceManagerLoader;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class XmlPersistenceHandler extends StrolchComponent implements StrolchPersistenceHandler {

	public static final String DB_STORE_PATH = "dbStore/"; //$NON-NLS-1$
	private PersistenceManager persistenceManager;

	public XmlPersistenceHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration componentConfiguration) {

		File basePathF = componentConfiguration.getRuntimeConfiguration().getRootPath();
		File dbStorePathF = new File(basePathF, DB_STORE_PATH);

		Properties properties = new Properties();
		properties.setProperty(PersistenceConstants.PROP_VERBOSE, "true"); //$NON-NLS-1$
		properties.setProperty(PersistenceConstants.PROP_XML_IO_MOD, IoMode.DOM.name());
		properties.setProperty(PersistenceConstants.PROP_BASEPATH, dbStorePathF.getAbsolutePath());

		this.persistenceManager = PersistenceManagerLoader.load(properties);

		this.persistenceManager.getCtxFactory().registerPersistenceContextFactory(Resource.class, Tags.RESOURCE,
				new ResourceContextFactory());
		this.persistenceManager.getCtxFactory().registerPersistenceContextFactory(Order.class, Tags.ORDER,
				new OrderContextFactory());
		
		super.initialize(componentConfiguration);
	}

	public StrolchTransaction openTx() {
		return openTx(PersistenceManager.DEFAULT_REALM);
	}

	@SuppressWarnings("resource")
	// caller will/must close
	public StrolchTransaction openTx(String realm) {
		PersistenceTransaction tx = this.persistenceManager.openTx(realm);
		XmlStrolchTransaction strolchTx = new XmlStrolchTransaction(tx);
		if (getContainer().hasComponent(ObserverHandler.class)) {
			strolchTx.setObserverHandler(getContainer().getComponent(ObserverHandler.class));
		}
		return strolchTx;
	}

	@Override
	public OrderDao getOrderDao(StrolchTransaction tx) {
		return new XmlOrderDao(tx);
	}

	@Override
	public ResourceDao getResourceDao(StrolchTransaction tx) {
		return new XmlResourceDao(tx);
	}
}
