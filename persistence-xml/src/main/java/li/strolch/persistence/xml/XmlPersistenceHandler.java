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

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.RealmHandler;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.agent.impl.StoreToDaoElementListener;
import li.strolch.model.ModelStatistics;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.Tags;
import li.strolch.model.activity.Activity;
import li.strolch.model.audit.Audit;
import li.strolch.model.log.LogMessage;
import li.strolch.model.xml.XmlModelSaxFileReader;
import li.strolch.persistence.api.*;
import li.strolch.persistence.xml.model.*;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import li.strolch.runtime.configuration.StrolchConfiguration;
import li.strolch.runtime.configuration.StrolchConfigurationException;
import li.strolch.utils.helper.StringHelper;
import li.strolch.xmlpers.api.*;

import java.io.File;
import java.util.*;

import static li.strolch.agent.impl.DefaultRealmHandler.PREFIX_DATA_STORE_FILE;
import static li.strolch.db.DbConstants.PROP_USE_ENV;
import static li.strolch.runtime.StrolchConstants.makeRealmKey;
import static li.strolch.utils.helper.StringHelper.isEmpty;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlPersistenceHandler extends StrolchComponent implements PersistenceHandler {

	public static final String PROP_DB_STORE_PATH = "dbStorePath";
	public static final String PROP_DB_IGNORE_REALM = "ignoreRealm";
	public static final String PROP_ALLOW_DATA_INIT_ON_EMPTY_DB = "allowDataInitOnEmptyDb";
	public static final String PROP_VERBOSE = "verbose";

	private Map<String, PersistenceStore> persistenceStoreMap;

	public XmlPersistenceHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public boolean supportsPaging() {
		return false;
	}

	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {

		this.persistenceStoreMap = new HashMap<>();

		Set<String> dbStorePaths = new HashSet<>();

		Set<String> realmNames = getContainer().getRealmNames();
		for (String realmName : realmNames) {

			StrolchRealm realm = getContainer().getRealm(realmName);
			if (realm.getMode().isTransient())
				continue;

			String dbUseEnvKey = makeRealmKey(realmName, PROP_USE_ENV, false);
			boolean dbUseEnv = configuration.getBoolean(dbUseEnvKey, false);

			String dbStorePathKey = makeRealmKey(realmName, PROP_DB_STORE_PATH, dbUseEnv);
			String dbVerboseKey = makeRealmKey(realmName, PROP_VERBOSE, dbUseEnv);

			String dbIgnoreRealmKey = makeRealmKey(realmName, PROP_DB_IGNORE_REALM, false);
			boolean dbIgnoreRealm = configuration.getBoolean(dbIgnoreRealmKey, false);
			if (dbIgnoreRealm) {
				logger.info("Ignoring any DB configuration for Realm {}", realmName);
				continue;
			}

			String dbStorePath = getConfigString(configuration, dbStorePathKey, dbUseEnv);
			boolean verbose = getConfigBoolean(configuration, dbVerboseKey, dbUseEnv);

			// validate URL
			if (dbStorePaths.contains(dbStorePath))
				throw new IllegalStateException(
						"The " + PROP_DB_STORE_PATH + " " + dbStorePath + " is already used by another realm!");
			dbStorePaths.add(dbStorePath);

			File basePathF = configuration.getRuntimeConfiguration().getDataPath();
			File dbStorePathF = new File(basePathF, dbStorePath);
			if (!dbStorePathF.exists() && !dbStorePathF.mkdir()) {
				throw new StrolchConfigurationException(
						"Could not create store path at " + dbStorePathF.getAbsolutePath());
			}

			// build a PersistenceManager
			Properties properties = new Properties();
			properties.setProperty(PersistenceConstants.PROP_VERBOSE, Boolean.toString(verbose));
			properties.setProperty(PersistenceConstants.PROP_XML_IO_MOD, IoMode.SAX.name());
			properties.setProperty(PersistenceConstants.PROP_BASEPATH, dbStorePathF.getAbsolutePath());
			PersistenceManager persistenceManager = PersistenceManagerLoader.load(properties);
			PersistenceContextFactoryDelegator ctxFactory = persistenceManager.getCtxFactory();
			ctxFactory.registerPersistenceContextFactory(Resource.class, Tags.RESOURCE, new ResourceContextFactory());
			ctxFactory.registerPersistenceContextFactory(Order.class, Tags.ORDER, new OrderContextFactory());
			ctxFactory.registerPersistenceContextFactory(Audit.class, Tags.AUDIT, new AuditContextFactory());
			ctxFactory.registerPersistenceContextFactory(Activity.class, Tags.ACTIVITY, new ActivityContextFactory());
			ctxFactory.registerPersistenceContextFactory(LogMessage.class, Tags.LOG_MESSAGE,
					new LogMessageContextFactory());

			PersistenceStore persistenceStore = new PersistenceStore();
			persistenceStore.dbStorePathF = dbStorePathF;
			persistenceStore.persistenceManager = persistenceManager;
			this.persistenceStoreMap.put(realmName, persistenceStore);
		}

		super.initialize(configuration);
	}

	private String getConfigString(ComponentConfiguration configuration, String dbKey, boolean useEnv) {
		if (!useEnv)
			return configuration.getString(dbKey, null);

		String value = System.getenv(dbKey);
		if (isEmpty(value))
			throw new IllegalStateException("Missing environment variable " + dbKey);
		return value;
	}

	private Boolean getConfigBoolean(ComponentConfiguration configuration, String dbKey, boolean useEnv) {
		if (!useEnv)
			return configuration.getBoolean(dbKey, false);

		String value = System.getenv(dbKey);
		if (isEmpty(value))
			throw new IllegalStateException("Missing environment variable " + dbKey);
		return StringHelper.parseBoolean(value);
	}

	@Override
	public void start() throws Exception {

		for (String realmName : this.persistenceStoreMap.keySet()) {

			String allowDataInitOnEmptyDbKey = makeRealmKey(realmName, PROP_ALLOW_DATA_INIT_ON_EMPTY_DB);
			boolean allowDataInitOnEmptyDb = getConfiguration().getBoolean(allowDataInitOnEmptyDbKey, Boolean.FALSE);

			PersistenceStore persistenceStore = this.persistenceStoreMap.get(realmName);

			File[] files = persistenceStore.dbStorePathF.listFiles();
			if (files == null)
				throw new IllegalStateException(persistenceStore.dbStorePathF.getAbsolutePath() + " does not exist!");
			if (files.length == 0 && allowDataInitOnEmptyDb) {
				logger.info("Initializing realm {} as DB is empty.", realmName);

				StrolchConfiguration strolchConfiguration = getContainer().getAgent().getStrolchConfiguration();
				ComponentConfiguration realmConfiguration = strolchConfiguration.getComponentConfiguration(
						RealmHandler.class.getSimpleName());
				String dataStoreKey = makeRealmKey(realmName, PREFIX_DATA_STORE_FILE);
				RuntimeConfiguration runtimeConfiguration = strolchConfiguration.getRuntimeConfiguration();
				File dataStoreF = realmConfiguration.getDataFile(dataStoreKey, null, runtimeConfiguration, true);

				runAsAgent(ctx -> {

					ModelStatistics statistics;
					try (StrolchTransaction tx = openTx(getContainer().getRealm(realmName), ctx.getCertificate(),
							getClass().getSimpleName(), false)) {

						StoreToDaoElementListener listener = new StoreToDaoElementListener(tx);
						XmlModelSaxFileReader handler = new XmlModelSaxFileReader(listener, dataStoreF, true);
						handler.parseFile();
						statistics = handler.getStatistics();
						tx.commitOnClose();
					}
					logger.info("Realm {} initialization statistics: {}", realmName, statistics);
				});
			}
		}

		super.start();
	}

	static class PersistenceStore {
		PersistenceManager persistenceManager;
		File dbStorePathF;
	}

	@Override
	public StrolchTransaction openTx(StrolchRealm realm, Certificate certificate, String action, boolean readOnly) {
		PersistenceStore persistenceStore = this.persistenceStoreMap.get(realm.getRealm());
		if (persistenceStore == null)
			throw new IllegalStateException("No XML persistence enabled for realm " + realm.getRealm());

		PersistenceTransaction tx = persistenceStore.persistenceManager.openTx();
		return new XmlStrolchTransaction(getContainer(), realm, certificate, action, readOnly, tx, this);
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
	public LogMessageDao getLogMessageDao(StrolchTransaction tx) {
		return new XmlLogMessageDao(tx);
	}
}
