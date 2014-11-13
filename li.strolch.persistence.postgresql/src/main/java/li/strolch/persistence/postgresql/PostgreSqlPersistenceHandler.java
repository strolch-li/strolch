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

import static ch.eitchnet.db.DbConstants.PROP_ALLOW_DATA_INIT_ON_SCHEMA_CREATE;
import static ch.eitchnet.db.DbConstants.PROP_ALLOW_SCHEMA_CREATION;
import static ch.eitchnet.db.DbConstants.PROP_ALLOW_SCHEMA_DROP;
import static ch.eitchnet.db.DbConstants.PROP_ALLOW_SCHEMA_MIGRATION;
import static li.strolch.agent.api.RealmHandler.SYSTEM_USER_DB_INITIALIZER;

import java.sql.Connection;
import java.text.MessageFormat;
import java.util.Map;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.RealmHandler;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.persistence.api.AuditDao;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.DbConnectionBuilder;
import li.strolch.runtime.configuration.StrolchConfiguration;
import li.strolch.runtime.configuration.StrolchConfigurationException;
import li.strolch.runtime.privilege.PrivilegeHandler;
import ch.eitchnet.db.DbConnectionCheck;
import ch.eitchnet.db.DbConnectionInfo;
import ch.eitchnet.db.DbDriverLoader;
import ch.eitchnet.db.DbException;
import ch.eitchnet.db.DbMigrationState;
import ch.eitchnet.db.DbSchemaVersionCheck;
import ch.eitchnet.privilege.model.Certificate;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PostgreSqlPersistenceHandler extends StrolchComponent implements PersistenceHandler {

	public static final String SCRIPT_PREFIX = "strolch";
	private ComponentConfiguration componentConfiguration;
	private Map<String, DbConnectionInfo> connetionInfoMap;

	public PostgreSqlPersistenceHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration componentConfiguration) {

		this.componentConfiguration = componentConfiguration;

		// server loader does not seem to work in all contexts, thus:
		org.postgresql.Driver.getLogLevel();

		DbConnectionBuilder connectionBuilder = new DbConnectionBuilder(getContainer(), componentConfiguration);
		Map<String, DbConnectionInfo> connectionInfoMap = connectionBuilder.build();
		for (DbConnectionInfo connectionInfo : connectionInfoMap.values()) {
			try {
				DbDriverLoader.loadDriverForConnection(connectionInfo);
			} catch (DbException e) {
				throw new StrolchConfigurationException("Could not load driver for connection "
						+ connectionInfo.getUrl(), e);
			}
		}

		this.connetionInfoMap = connectionInfoMap;
		super.initialize(componentConfiguration);
	}

	/**
	 * Returns the map of {@link DbConnectionInfo} which can be used in maintenance mode
	 * 
	 * @return the connetionInfoMap
	 */
	public Map<String, DbConnectionInfo> getConnetionInfoMap() {
		return this.connetionInfoMap;
	}

	@Override
	public void start() {

		// test all connections
		DbConnectionCheck connectionCheck = new DbConnectionCheck(this.connetionInfoMap);
		try {
			connectionCheck.checkConnections();
		} catch (DbException e) {
			throw new StrolchConfigurationException("At least one connection failed: " + e.getMessage(), e);
		}

		boolean allowSchemaCreation = componentConfiguration.getBoolean(PROP_ALLOW_SCHEMA_CREATION, Boolean.FALSE);
		boolean allowSchemaMigration = componentConfiguration.getBoolean(PROP_ALLOW_SCHEMA_MIGRATION, Boolean.FALSE);
		boolean allowSchemaDrop = componentConfiguration.getBoolean(PROP_ALLOW_SCHEMA_DROP, Boolean.FALSE);
		boolean allowDataInitOnSchemaCreate = componentConfiguration.getBoolean(PROP_ALLOW_DATA_INIT_ON_SCHEMA_CREATE,
				Boolean.FALSE);

		DbSchemaVersionCheck schemaVersionCheck = new DbSchemaVersionCheck(SCRIPT_PREFIX, this.getClass(),
				allowSchemaCreation, allowSchemaMigration, allowSchemaDrop);
		try {
			schemaVersionCheck.checkSchemaVersion(this.connetionInfoMap);
		} catch (DbException e) {
			throw new StrolchConfigurationException(
					"Failed to validate the schema for a connection: " + e.getMessage(), e);
		}

		// if allowed, perform DB initialization
		if (!allowSchemaCreation || !allowSchemaDrop || !allowDataInitOnSchemaCreate) {
			logger.info("Data Initialization not enabled, so not checking if needed.");
		} else {
			Map<String, DbMigrationState> dbMigrationStates = schemaVersionCheck.getDbMigrationStates();
			String msg = "Data Initialization is enabled, checking for {0} realms if DB initialization is required...";
			logger.info(MessageFormat.format(msg, dbMigrationStates.size()));
			PrivilegeHandler privilegeHandler = getContainer().getPrivilegeHandler();
			StrolchAgent agent = getContainer().getAgent();
			PostgreSqlSchemaInitializer schemaInitializer = new PostgreSqlSchemaInitializer(agent, this,
					dbMigrationStates);
			privilegeHandler.runAsSystem(SYSTEM_USER_DB_INITIALIZER, schemaInitializer);
		}

		super.start();
	}

	@Override
	public StrolchTransaction openTx(StrolchRealm realm, Certificate certificate, String action) {
		return new PostgreSqlStrolchTransaction(getContainer().getPrivilegeHandler(), realm, certificate, action, this);
	}

	Connection getConnection(String realm) {
		DbConnectionInfo dbInfo = this.connetionInfoMap.get(realm);
		if (dbInfo == null) {
			String msg = MessageFormat.format("There is no connection registered for the realm {0}", realm); //$NON-NLS-1$
			throw new StrolchPersistenceException(msg);
		}

		try {
			return dbInfo.openConnection();
		} catch (DbException e) {
			throw new StrolchPersistenceException("Failed to open a connection to " + dbInfo + " due to "
					+ e.getMessage(), e);
		}
	}

	@Override
	public OrderDao getOrderDao(StrolchTransaction tx) {
		return ((PostgreSqlStrolchTransaction) tx).getOrderDao();
	}

	@Override
	public ResourceDao getResourceDao(StrolchTransaction tx) {
		return ((PostgreSqlStrolchTransaction) tx).getResourceDao();
	}

	@Override
	public AuditDao getAuditDao(StrolchTransaction tx) {
		return ((PostgreSqlStrolchTransaction) tx).getAuditDao();
	}

	@Override
	public void performDbInitialization() {
		ComponentContainer container = getContainer();
		StrolchAgent agent = container.getAgent();
		PrivilegeHandler privilegeHandler = container.getPrivilegeHandler();
		StrolchConfiguration strolchConfiguration = this.getContainer().getAgent().getStrolchConfiguration();
		PostgreSqlDbInitializer sqlDbInitializer = new PostgreSqlDbInitializer(agent, this,
				strolchConfiguration.getComponentConfiguration(getName()));
		privilegeHandler.runAsSystem(RealmHandler.SYSTEM_USER_DB_INITIALIZER, sqlDbInitializer);
	}
}
