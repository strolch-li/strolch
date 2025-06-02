/*
 * Copyright (c) 2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.agent.impl.eclipsestore;

import li.strolch.agent.api.*;
import li.strolch.agent.impl.DataStoreMode;
import li.strolch.agent.impl.InternalStrolchRealm;
import li.strolch.agent.impl.TransientTransaction;
import li.strolch.agent.impl.XmlModelLoader;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.StrolchConfigurationException;
import li.strolch.utils.dbc.DBC;
import org.eclipse.store.afs.nio.types.NioFileSystem;
import org.eclipse.store.storage.embedded.types.EmbeddedStorageFoundation;
import org.eclipse.store.storage.types.Storage;
import org.eclipse.store.storage.types.StorageBackupSetup;
import org.eclipse.store.storage.types.StorageChannelCountProvider;
import org.eclipse.store.storage.types.StorageConfiguration;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static java.lang.System.currentTimeMillis;
import static java.text.MessageFormat.format;
import static li.strolch.agent.impl.DefaultRealmHandler.PREFIX_DATA_STORE_FILE;
import static li.strolch.db.DbConstants.PROP_ALLOW_DATA_INIT_ON_SCHEMA_CREATE;
import static li.strolch.model.Tags.*;
import static li.strolch.runtime.StrolchConstants.makeRealmKey;
import static li.strolch.utils.helper.StringHelper.formatMillisecondsDuration;

public class EclipseStoreRealm extends InternalStrolchRealm {

	public static final String PROP_DB_STORE = "dbStore";
	public static final String PROP_ENABLE_BACKUP = "enableBackup";
	public static final String BACKUP = "Backup";

	private File storageDir;

	private EclipseStoreResourceMap resourceMap;
	private EclipseStoreOrderMap orderMap;
	private EclipseStoreActivityMap activityMap;
	private EclipseStoreAuditTrail auditTrail;
	private boolean verbose;
	private File modelFile;
	private boolean allowDataInitOnSchemaCreate;
	private boolean enableBackup;

	public EclipseStoreRealm(String realm) {
		super(realm);
	}

	@Override
	public DataStoreMode getMode() {
		return DataStoreMode.ECLIPSE_STORE;
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
	public StrolchTransaction openTx(Certificate certificate, String action, boolean readOnly) {
		DBC.PRE.assertEquals("Realm is not in state started!", ComponentState.STARTED, getState());
		DBC.PRE.assertNotNull("Certificate must be set!", certificate);
		//noinspection resource
		return new TransientTransaction(this.container, this, certificate, action, readOnly).suppressAuditsForAudits();
	}

	@Override
	public void initialize(ComponentContainer container, ComponentConfiguration config) {
		super.initialize(container, config);

		this.verbose = config.isVerbose();
		this.allowDataInitOnSchemaCreate = config.getBoolean(PROP_ALLOW_DATA_INIT_ON_SCHEMA_CREATE, false);
		if (this.allowDataInitOnSchemaCreate) {
			String dataStoreFile = makeRealmKey(getRealm(), PREFIX_DATA_STORE_FILE);
			if (!config.hasProperty(dataStoreFile)) {
				String msg
						= "There is no data store file for realm {0}. Set a property with key {1} when allowing to init data on schema creation";
				msg = format(msg, getRealm(), dataStoreFile);
				throw new StrolchConfigurationException(msg);
			}
			this.modelFile = config.getDataFile(dataStoreFile, null, config.getRuntimeConfiguration(), true);
		}

		String enableBackupKey = makeRealmKey(getRealm(), PROP_ENABLE_BACKUP);
		this.enableBackup = config.getBoolean(enableBackupKey, false);

		String dbStoreKey = makeRealmKey(getRealm(), PROP_DB_STORE);
		assertKeyExists(config, dbStoreKey, PROP_DB_STORE);
		this.storageDir = config.getDataDir(dbStoreKey, null, config.getRuntimeConfiguration(), false);
		if (!this.storageDir.exists()) {
			try {
				Files.createDirectory(this.storageDir.toPath());
			} catch (IOException e) {
				throw new IllegalStateException(
						"Failed to create storage directory " + this.storageDir.getAbsolutePath(), e);
			}
		}
	}

	@Override
	public void start(PrivilegeContext privilegeContext) {
		super.start(privilegeContext);

		long start = currentTimeMillis();
		logger.info("Initializing Eclipse Store Realm {} using storage directory {}...", getRealm(), this.storageDir);

		long startStep;
		try {
			startStep = currentTimeMillis();
			EmbeddedStorageFoundation<?> foundation = buildFoundation(RESOURCE);
			this.resourceMap = new EclipseStoreResourceMap(getRealm(), foundation.createEmbeddedStorageManager());
			logger.info("Initialized Resource Storage in {}",
					formatMillisecondsDuration(currentTimeMillis() - startStep));
			startStep = currentTimeMillis();
			this.orderMap = new EclipseStoreOrderMap(getRealm(), buildFoundation(ORDER).createEmbeddedStorageManager());
			logger.info("Initialized Order Storage in {}", formatMillisecondsDuration(currentTimeMillis() - startStep));
			startStep = currentTimeMillis();
			this.activityMap = new EclipseStoreActivityMap(getRealm(),
					buildFoundation(ACTIVITY).createEmbeddedStorageManager());
			logger.info("Initialized Activity Storage in {}",
					formatMillisecondsDuration(currentTimeMillis() - startStep));
			startStep = currentTimeMillis();
			this.auditTrail = new EclipseStoreAuditTrail(getRealm(),
					buildFoundation(AUDIT).createEmbeddedStorageManager());
			logger.info("Initialized Audit Storage in {}", formatMillisecondsDuration(currentTimeMillis() - startStep));
		} catch (Exception e) {
			throw new IllegalStateException(
					format("Failed to configure storages for realm {0} in storage directory {1}", getRealm(),
							this.storageDir.getAbsolutePath()), e);
		}

		try {
			this.resourceMap.start();
			this.orderMap.start();
			this.activityMap.start();
			this.auditTrail.start();
		} catch (Exception e) {
			throw new IllegalStateException("Failed to start storages for realm " + getRealm(), e);
		}

		if (this.resourceMap.getStorageManager().root() == null) {
			if (!this.allowDataInitOnSchemaCreate) {
				logger.info("First load of storage, but data init not allowed thus only initializing");
				initializeStorageManager();
				storeRoot();
			} else {
				try {
					logger.info("First load of storage, thus loading the model from {}", this.modelFile.getName());
					initializeStorageManager();
					loadModelIntoStorage(privilegeContext);
					storeRoot();
				} catch (Exception e) {
					throw new IllegalStateException(
							"Failed to load model from " + this.modelFile.getName() + " for realm " + getRealm(), e);
				}
			}
		}

		logger.info("Initialized Eclipse Store for realm {} in {}", getRealm(),
				formatMillisecondsDuration(currentTimeMillis() - start));
	}

	private void loadModelIntoStorage(PrivilegeContext privilegeContext) {
		long startStep;
		startStep = currentTimeMillis();
		XmlModelLoader loader = new XmlModelLoader(getRealm(), this.verbose, this.modelFile);
		loader.load(privilegeContext, this);
		logger.info("Loaded model into storage in {}", formatMillisecondsDuration(currentTimeMillis() - startStep));
	}

	private void storeRoot() {
		this.resourceMap.storeRoot();
		this.orderMap.storeRoot();
		this.activityMap.storeRoot();
		this.auditTrail.storeRoot();
	}

	private void initializeStorageManager() {
		long startStep = currentTimeMillis();
		this.resourceMap.initStorageManager();
		this.orderMap.initStorageManager();
		this.activityMap.initStorageManager();
		this.auditTrail.initStorageManager();
		logger.info("Stored initial storages in {}", formatMillisecondsDuration(currentTimeMillis() - startStep));
	}

	private EmbeddedStorageFoundation<?> buildFoundation(String databaseName) {
		Path databasePath = new File(this.storageDir, databaseName).toPath();
		NioFileSystem fileSystem = NioFileSystem.New();

		int channelCount = 2;
		StorageConfiguration.Builder<?> storageConfigurationBuilder = StorageConfiguration
				.Builder()
				.setStorageFileProvider(Storage
						.FileProviderBuilder(fileSystem)
						.setDirectory(fileSystem.ensureDirectory(databasePath))
						.createFileProvider())
				.setChannelCountProvider(StorageChannelCountProvider.New(channelCount));

		if (this.enableBackup) {
			Path backupPath = new File(this.storageDir, databaseName + BACKUP).toPath();
			storageConfigurationBuilder.setBackupSetup(StorageBackupSetup.New(fileSystem.ensureDirectory(backupPath)));
		}

		return EmbeddedStorageFoundation
				.New()
				.setDataBaseName(databaseName)
				.setConfiguration(storageConfigurationBuilder.createConfiguration());
	}

	@Override
	public void stop() {
		try {
			if (this.resourceMap != null)
				this.resourceMap.stop();
			if (this.orderMap != null)
				this.orderMap.stop();
			if (this.activityMap != null)
				this.activityMap.stop();
			if (this.auditTrail != null)
				this.auditTrail.stop();
		} catch (Exception e) {
			logger.error("Failed to stop storages for realm {}", getRealm(), e);
		}

		super.stop();
	}

	@Override
	public void destroy() {
		try {
			if (this.resourceMap != null)
				this.resourceMap.destroy();
			if (this.orderMap != null)
				this.orderMap.destroy();
			if (this.activityMap != null)
				this.activityMap.destroy();
			if (this.auditTrail != null)
				this.auditTrail.destroy();
		} catch (Exception e) {
			logger.error("Failed to destroy storages for realm {}", getRealm(), e);
		}

		super.destroy();
	}
}
