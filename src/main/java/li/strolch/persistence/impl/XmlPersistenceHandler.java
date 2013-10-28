/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the ch.eitchnet.persistence.impl.
 *
 *  ch.eitchnet.persistence.impl is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  ch.eitchnet.persistence.impl is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with ch.eitchnet.persistence.impl.  If not, see 
 *  <http://www.gnu.org/licenses/>.
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
import li.strolch.runtime.component.StrolchComponent;
import li.strolch.runtime.configuration.ComponentConfiguration;
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

	public XmlPersistenceHandler() {
		super(StrolchPersistenceHandler.class.getName());
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
	}

	public StrolchTransaction openTx() {
		return openTx(PersistenceManager.DEFAULT_REALM);
	}

	@SuppressWarnings("resource")
	// caller must close
	public StrolchTransaction openTx(String realm) {
		PersistenceTransaction tx = this.persistenceManager.openTx(realm);
		XmlStrolchTransaction strolchTx = new XmlStrolchTransaction(tx);
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
