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
package li.strolch.persistence.postgresql;

import static li.strolch.agent.impl.DefaultRealmHandler.PREFIX_DATA_STORE_FILE;
import static li.strolch.runtime.StrolchConstants.makeRealmKey;

import java.io.File;
import java.text.MessageFormat;

import li.strolch.agent.api.RealmHandler;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.impl.StoreToDaoElementListener;
import li.strolch.db.DbMigrationState;
import li.strolch.model.ModelStatistics;
import li.strolch.model.xml.XmlModelSaxFileReader;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.handler.SystemAction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import li.strolch.runtime.configuration.StrolchConfiguration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class PostgreSqlInitializer extends SystemAction {

	protected static final Logger logger = LoggerFactory.getLogger(PostgreSqlInitializer.class);
	protected final StrolchAgent agent;
	protected final PostgreSqlPersistenceHandler persistenceHandler;
	protected final RuntimeConfiguration runtimeConfig;
	protected final ComponentConfiguration realmConfig;

	public PostgreSqlInitializer(StrolchAgent agent, PostgreSqlPersistenceHandler persistenceHandler) {
		this.agent = agent;
		this.persistenceHandler = persistenceHandler;
		StrolchConfiguration strolchConfiguration = agent.getStrolchConfiguration();
		this.runtimeConfig = strolchConfiguration.getRuntimeConfiguration();
		this.realmConfig = strolchConfiguration.getComponentConfiguration(RealmHandler.class.getSimpleName());
	}

	protected abstract Certificate getCertificate();

	protected void initSchemaFromDataStore(DbMigrationState migrationType, String realmName) {
		boolean needsDbInit = checkNeedsDbInit(migrationType);
		if (!needsDbInit) {
			logger.info("Schema for realm {} had no migration run. No need for data initialization.", realmName);
			return;
		}

		logger.info("Migration for schema for realm {} was {} so need to initialize the data from the databaseStore...",
				realmName, migrationType);

		ModelStatistics statistics;
		try (StrolchTransaction tx = this.persistenceHandler.openTx(this.agent.getContainer().getRealm(realmName),
				getCertificate(), getClass().getSimpleName(), false)) {
			File dataStoreF = getDataStoreFile(this.runtimeConfig, this.realmConfig, realmName);

			StoreToDaoElementListener listener = new StoreToDaoElementListener(tx);
			XmlModelSaxFileReader handler = new XmlModelSaxFileReader(listener, dataStoreF, true);
			handler.parseFile();
			statistics = handler.getStatistics();
			tx.commitOnClose();
		}
		logger.info("Realm {} initialization statistics: {}", realmName, statistics);
	}

	protected boolean checkNeedsDbInit(DbMigrationState migrationType) {
		return switch (migrationType) {
			case CREATED, DROPPED_CREATED -> true;
			case MIGRATED, NOTHING -> false;
		};
	}

	protected File getDataStoreFile(RuntimeConfiguration runtimeConfiguration,
			ComponentConfiguration realmConfiguration, String realmName) {
		String dataStoreKey = makeRealmKey(realmName, PREFIX_DATA_STORE_FILE, false);
		return realmConfiguration.getDataFile(dataStoreKey, null, runtimeConfiguration, true);
	}
}
