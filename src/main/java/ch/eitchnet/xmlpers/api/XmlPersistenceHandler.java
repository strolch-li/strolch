/*
 * Copyright (c) 2012
 * 
 * This file is part of ch.eitchnet.java.xmlpers
 *
 * ch.eitchnet.java.xmlpers is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * ch.eitchnet.java.xmlpers is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ch.eitchnet.java.xmlpers.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.xmlpers.api;

import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.PropertiesHelper;
import ch.eitchnet.xmlpers.impl.XmlPersistenceFileDao;
import ch.eitchnet.xmlpers.impl.XmlPersistencePathBuilder;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class XmlPersistenceHandler {

	protected static final Logger logger = LoggerFactory.getLogger(XmlPersistenceHandler.class);

	protected boolean initialized;
	protected boolean verbose;
	protected XmlPersistenceDaoFactory daoFactory;

	public void initialize(Properties properties) {
		if (this.initialized)
			throw new IllegalStateException("Already initialized!");

		// get properties
		String context = XmlPersistenceHandler.class.getSimpleName();
		boolean verbose = PropertiesHelper.getPropertyBool(properties, context, XmlPersistenceConstants.PROP_VERBOSE,
				Boolean.FALSE).booleanValue();
		String daoFactoryClassName = PropertiesHelper.getProperty(properties, context,
				XmlPersistenceConstants.PROP_DAO_FACTORY_CLASS, null);

		// load dao factory
		XmlPersistenceDaoFactory daoFactory;
		try {
			@SuppressWarnings("unchecked")
			Class<XmlPersistenceDaoFactory> xmlDaoFactoryClass = (Class<XmlPersistenceDaoFactory>) Class.forName(daoFactoryClassName);

			daoFactory = xmlDaoFactoryClass.newInstance();

		} catch (ClassNotFoundException e) {
			throw new XmlPersistenceException("XmlDaoFactory class does not exist " + daoFactoryClassName, e);
		} catch (Exception e) {
			throw new XmlPersistenceException("Failed to load class " + daoFactoryClassName, e);
		}

		// initialize the dao factory
		XmlPersistencePathBuilder pathBuilder = new XmlPersistencePathBuilder(properties);
		XmlPersistenceFileDao fileDao = new XmlPersistenceFileDao(pathBuilder, properties);
		daoFactory.initialize(fileDao, properties);

		this.daoFactory = daoFactory;
		this.verbose = verbose;
	}

	public XmlPersistenceTransaction openTx() {

		XmlPersistenceTransaction tx = new XmlPersistenceTransaction(this.daoFactory, this.verbose);
		XmlPersistenceTransaction.setTx(tx);
		return tx;
	}
}
