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
package li.strolch.xmlpers.impl;

import java.io.File;
import java.lang.reflect.Field;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.utils.helper.PropertiesHelper;
import li.strolch.utils.helper.StringHelper;
import li.strolch.xmlpers.api.IoMode;
import li.strolch.xmlpers.api.PersistenceConstants;
import li.strolch.xmlpers.api.PersistenceContextFactoryDelegator;
import li.strolch.xmlpers.api.PersistenceManager;
import li.strolch.xmlpers.api.PersistenceTransaction;
import li.strolch.xmlpers.api.XmlPersistenceException;
import li.strolch.xmlpers.objref.LockableObject;
import li.strolch.xmlpers.objref.ObjectReferenceCache;

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
