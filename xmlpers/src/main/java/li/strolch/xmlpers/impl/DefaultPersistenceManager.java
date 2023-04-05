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

import static li.strolch.utils.helper.PropertiesHelper.*;
import static li.strolch.xmlpers.api.PersistenceConstants.*;

import java.io.File;
import java.text.MessageFormat;
import java.util.Properties;

import li.strolch.xmlpers.api.*;
import li.strolch.utils.concurrent.LockableObject;
import li.strolch.xmlpers.objref.ObjectReferenceCache;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultPersistenceManager implements PersistenceManager {

	protected static final Logger logger = LoggerFactory.getLogger(DefaultPersistenceManager.class);

	protected boolean initialized;
	protected boolean verbose;
	protected boolean allowOverwriteOnCreate;
	protected IoMode ioMode;
	private PersistenceContextFactoryDelegator ctxFactory;
	private ObjectReferenceCache objectRefCache;
	private PathBuilder pathBuilder;

	public void initialize(Properties properties) {
		if (this.initialized)
			throw new IllegalStateException("Already initialized!");

		String context = DefaultPersistenceManager.class.getSimpleName();

		// get properties
		boolean verbose = getPropertyBool(properties, context, PROP_VERBOSE, Boolean.FALSE);
		boolean allowOverwriteOnCreate = getPropertyBool(properties, context, PROP_ALLOW_OVERWRITE_ON_CREATE,
				Boolean.TRUE);
		IoMode ioMode = IoMode.valueOf(getProperty(properties, context, PROP_XML_IO_MOD, IoMode.DOM.name()));
		long lockTime = getPropertyLong(properties, context, PROP_LOCK_TIME_MILLIS, LockableObject.getLockTime());
		String basePath = getProperty(properties, context, PROP_BASEPATH, null);

		// set lock time on LockableObject
		if (lockTime != LockableObject.getLockTime())
			LockableObject.setTryLockTime(lockTime);

		// validate base path exists and is writable
		File basePathF = new File(basePath).getAbsoluteFile();
		if (!basePathF.exists())
			throw new XmlPersistenceException(
					MessageFormat.format("The database store path does not exist at {0}",
							basePathF.getAbsolutePath()));
		if (!basePathF.canWrite())
			throw new XmlPersistenceException(
					MessageFormat.format("The database store path is not writeable at {0}",
							basePathF.getAbsolutePath()));
		logger.info(MessageFormat.format("Using base path {0}", basePathF));

		this.verbose = verbose;
		this.allowOverwriteOnCreate = allowOverwriteOnCreate;
		this.ioMode = ioMode;
		this.ctxFactory = new PersistenceContextFactoryDelegator();

		this.pathBuilder = new PathBuilder(basePathF);
		this.objectRefCache = new ObjectReferenceCache();
	}

	@Override
	public PersistenceContextFactoryDelegator getCtxFactory() {
		return this.ctxFactory;
	}

	@Override
	public ObjectReferenceCache getObjectRefCache() {
		return this.objectRefCache;
	}

	@Override
	public PathBuilder getPathBuilder() {
		return this.pathBuilder;
	}

	@Override
	public IoMode getIoMode() {
		return this.ioMode;
	}

	@Override
	public synchronized PersistenceTransaction openTx() {
		return new DefaultPersistenceTransaction(this, this.ioMode, this.verbose, this.allowOverwriteOnCreate);
	}
}
