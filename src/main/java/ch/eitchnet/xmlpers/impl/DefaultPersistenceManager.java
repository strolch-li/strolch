/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package ch.eitchnet.xmlpers.impl;

import java.io.File;
import java.lang.reflect.Field;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.PropertiesHelper;
import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.xmlpers.api.IoMode;
import ch.eitchnet.xmlpers.api.PersistenceConstants;
import ch.eitchnet.xmlpers.api.PersistenceContextFactoryDelegator;
import ch.eitchnet.xmlpers.api.PersistenceManager;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import ch.eitchnet.xmlpers.api.XmlPersistenceException;
import ch.eitchnet.xmlpers.objref.LockableObject;
import ch.eitchnet.xmlpers.objref.ObjectReferenceCache;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class DefaultPersistenceManager implements PersistenceManager {

	protected static final Logger logger = LoggerFactory.getLogger(DefaultPersistenceManager.class);

	protected boolean initialized;
	protected boolean verbose;
	protected IoMode defaultIoMode;
	protected Properties properties;
	protected Map<String, DefaultPersistenceRealm> realmMap;
	private PersistenceContextFactoryDelegator ctxFactory;

	public void initialize(Properties properties) {
		if (this.initialized)
			throw new IllegalStateException("Already initialized!"); //$NON-NLS-1$

		String context = DefaultPersistenceManager.class.getSimpleName();

		// get properties
		boolean verbose = PropertiesHelper.getPropertyBool(properties, context, PersistenceConstants.PROP_VERBOSE,
				Boolean.FALSE).booleanValue();
		String ioModeS = PropertiesHelper.getProperty(properties, context, PersistenceConstants.PROP_XML_IO_MOD,
				IoMode.DOM.name());
		IoMode ioMode = IoMode.valueOf(ioModeS);
		long lockTime = PropertiesHelper.getPropertyLong(properties, context,
				PersistenceConstants.PROP_LOCK_TIME_MILLIS, 10000L);

		// set lock time on LockableObject
		try {
			Field lockTimeField = LockableObject.class.getDeclaredField("tryLockTime");//$NON-NLS-1$
			lockTimeField.setAccessible(true);
			lockTimeField.setLong(null, lockTime);
			logger.info("Using a max lock acquire time of " + StringHelper.formatMillisecondsDuration(lockTime)); //$NON-NLS-1$
		} catch (SecurityException | NoSuchFieldException | IllegalArgumentException | IllegalAccessException e) {
			throw new RuntimeException("Failed to configure tryLockTime on LockableObject!", e); //$NON-NLS-1$
		}

		// validate base path
		validateBasePath(properties);

		this.properties = properties;
		this.verbose = verbose;
		this.defaultIoMode = ioMode;
		this.realmMap = new HashMap<>();
		this.ctxFactory = new PersistenceContextFactoryDelegator();
	}

	private void validateBasePath(Properties properties) {
		String context = DefaultPersistenceManager.class.getSimpleName();
		String basePath = PropertiesHelper.getProperty(properties, context, PersistenceConstants.PROP_BASEPATH, null);

		// validate base path exists and is writable
		File basePathF = new File(basePath);
		if (!basePathF.exists())
			throw new XmlPersistenceException(MessageFormat.format("The database store path does not exist at {0}", //$NON-NLS-1$
					basePathF.getAbsolutePath()));
		if (!basePathF.canWrite())
			throw new XmlPersistenceException(MessageFormat.format("The database store path is not writeable at {0}", //$NON-NLS-1$
					basePathF.getAbsolutePath()));
	}

	@Override
	public PersistenceContextFactoryDelegator getCtxFactory() {
		return this.ctxFactory;
	}

	@Override
	public PersistenceTransaction openTx() {
		return openTx(DEFAULT_REALM);
	}

	@Override
	public synchronized PersistenceTransaction openTx(String realmName) {

		DefaultPersistenceRealm persistenceRealm = this.realmMap.get(realmName);
		if (persistenceRealm == null) {

			PathBuilder pathBuilder = new PathBuilder(realmName, this.properties);
			ObjectReferenceCache objectRefCache = new ObjectReferenceCache(realmName);
			persistenceRealm = new DefaultPersistenceRealm(realmName, this, this.ctxFactory, pathBuilder,
					objectRefCache);

			this.realmMap.put(realmName, persistenceRealm);
		}

		PersistenceTransaction tx = new DefaultPersistenceTransaction(persistenceRealm, this.verbose);
		tx.setIoMode(this.defaultIoMode);
		return tx;
	}
}
