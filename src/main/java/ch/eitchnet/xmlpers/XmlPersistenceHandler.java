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
package ch.eitchnet.xmlpers;

import org.apache.log4j.Logger;

import ch.eitchnet.utils.helper.SystemHelper;
import ch.eitchnet.utils.objectfilter.ITransactionObject;
import ch.eitchnet.utils.objectfilter.ObjectFilter;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class XmlPersistenceHandler {

	/**
	 * 
	 */
	public static final String CONFIG_VERBOSE = "ch.eitchnet.xmlpers.config.verbose";

	/**
	 * 
	 */
	public static final String CONFIG_BASEPATH = "ch.eitchnet.xmlpers.config.basepath";

	/**
	 * 
	 */
	public static final String CONFIG_DAO_FACTORY_CLASS = "ch.eitchnet.xmlpers.config.daoFactoryClass";

	protected static final Logger logger = Logger.getLogger(XmlPersistenceHandler.class);

	protected boolean verbose;
	protected ThreadLocal<XmlPersistenceTransaction> xmlPersistenceTxThreadLocal;
	protected XmlFilePersister persister;
	protected XmlDaoFactory xmlDaoFactory;

	/**
	 * 
	 */
	public void initialize() {

		String basePath = SystemHelper.getProperty(XmlPersistenceHandler.class.getSimpleName(), CONFIG_BASEPATH, null);
		verbose = SystemHelper.getPropertyBool(XmlPersistenceHandler.class.getSimpleName(), CONFIG_VERBOSE,
				Boolean.FALSE).booleanValue();

		// get class to use as transaction
		String daoFactoryClassName = SystemHelper.getProperty(XmlPersistenceHandler.class.getSimpleName(),
				CONFIG_DAO_FACTORY_CLASS, null);
		try {
			@SuppressWarnings("unchecked")
			Class<XmlDaoFactory> xmlDaoFactoryClass = (Class<XmlDaoFactory>) Class.forName(daoFactoryClassName);

			xmlDaoFactory = xmlDaoFactoryClass.newInstance();

		} catch (ClassNotFoundException e) {
			throw new XmlPersistenceExecption("XmlDaoFactory class does not exist " + daoFactoryClassName, e);
		} catch (Exception e) {
			throw new XmlPersistenceExecption("Failed to load class " + daoFactoryClassName, e);
		}

		XmlPersistencePathBuilder pathBuilder = new XmlPersistencePathBuilder(basePath);
		persister = new XmlFilePersister(pathBuilder, verbose);

		// initialize the Thread local object which is used per transaction
		xmlPersistenceTxThreadLocal = new ThreadLocal<XmlPersistenceTransaction>();
	}

	/**
	 * 
	 */
	public XmlPersistenceTransaction openTx() {

		if (verbose)
			logger.info("Opening new transaction...");

		// make sure no previous filter exists
		XmlPersistenceTransaction xmlPersistenceTx = this.xmlPersistenceTxThreadLocal.get();
		if (xmlPersistenceTx != null)
			throw new XmlPersistenceExecption("Previous transaction not properly closed");

		// set a new persistence transaction object
		ObjectFilter<ITransactionObject> objectFilter = new ObjectFilter<ITransactionObject>();
		xmlPersistenceTx = new XmlPersistenceTransaction();
		xmlPersistenceTx.initialize(persister, xmlDaoFactory, objectFilter, verbose);

		this.xmlPersistenceTxThreadLocal.set(xmlPersistenceTx);

		return xmlPersistenceTx;
	}

	/**
	 * 
	 */
	public XmlPersistenceTransaction getTx() {
		XmlPersistenceTransaction xmlPersistenceTx = this.xmlPersistenceTxThreadLocal.get();
		if (xmlPersistenceTx == null)
			throw new XmlPersistenceExecption("No transaction currently open!");

		return xmlPersistenceTx;
	}

	/**
	 * 
	 */
	public void commitTx() {

		if (verbose)
			logger.info("Committing transaction...");

		try {
			XmlPersistenceTransaction xmlPersistenceTx = this.xmlPersistenceTxThreadLocal.get();
			if (xmlPersistenceTx == null)
				throw new XmlPersistenceExecption("No transaction currently open!");

			xmlPersistenceTx.commitTx();
		} finally {
			this.xmlPersistenceTxThreadLocal.set(null);
		}
	}
}
