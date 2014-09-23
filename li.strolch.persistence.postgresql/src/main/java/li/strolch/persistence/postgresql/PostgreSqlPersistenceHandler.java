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

import static li.strolch.agent.api.RealmHandler.SYSTEM_USER_DB_INITIALIZER;
import static li.strolch.runtime.StrolchConstants.makeRealmKey;

import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.RealmHandler;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.persistence.api.AuditDao;
import li.strolch.persistence.api.DbConnectionInfo;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.StrolchConfiguration;
import li.strolch.runtime.configuration.StrolchConfigurationException;
import li.strolch.runtime.privilege.PrivilegeHandler;
import ch.eitchnet.privilege.model.Certificate;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PostgreSqlPersistenceHandler extends StrolchComponent implements PersistenceHandler {

	public static final String PROP_DB_URL = "db.url"; //$NON-NLS-1$
	public static final String PROP_DB_USERNAME = "db.username"; //$NON-NLS-1$
	public static final String PROP_DB_PASSWORD = "db.password"; //$NON-NLS-1$
	public static final String PROP_ALLOW_SCHEMA_CREATION = "allowSchemaCreation";
	public static final String PROP_ALLOW_SCHEMA_DROP = "allowSchemaDrop";
	public static final String PROP_ALLOW_DATA_INIT_ON_SCHEMA_CREATE = "allowDataInitOnSchemaCreate";
	public static final String PROP_DB_VERSION = "db_version";
	public static final String RESOURCE_DB_VERSION = "/db_version.properties";

	private ComponentConfiguration componentConfiguration;
	private Map<String, DbConnectionInfo> connetionInfoMap;

	public PostgreSqlPersistenceHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration componentConfiguration) {

		this.componentConfiguration = componentConfiguration;
		this.connetionInfoMap = new HashMap<>();

		Set<String> realmNames = getContainer().getRealmNames();
		for (String realmName : realmNames) {

			StrolchRealm realm = getContainer().getRealm(realmName);
			if (realm.getMode().isTransient())
				continue;

			String dbUrlKey = makeRealmKey(realmName, PROP_DB_URL);
			String dbUsernameKey = makeRealmKey(realmName, PROP_DB_USERNAME);
			String dbPasswordKey = makeRealmKey(realmName, PROP_DB_PASSWORD);

			String dbUrl = componentConfiguration.getString(dbUrlKey, null);
			String username = componentConfiguration.getString(dbUsernameKey, null);
			String password = componentConfiguration.getString(dbPasswordKey, null);

			DbConnectionInfo connectionInfo = new DbConnectionInfo(realmName, dbUrl);
			connectionInfo.setUsername(username);
			connectionInfo.setPassword(password);

			loadDriverForConnection(connectionInfo);
			this.connetionInfoMap.put(realmName, connectionInfo);
		}

		super.initialize(componentConfiguration);
	}

	private void loadDriverForConnection(DbConnectionInfo connectionInfo) {
		Driver driver;
		try {
			// server loader does not seem to work in all contexts, thus:
			org.postgresql.Driver.getLogLevel();

			driver = DriverManager.getDriver(connectionInfo.getUrl());
		} catch (SQLException e) {
			String msg = "Failed to load DB driver for URL {0} due to: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, connectionInfo.getUrl(), e.getMessage());
			throw new StrolchConfigurationException(msg, e);
		}

		String compliant = driver.jdbcCompliant() ? "" : "non"; //$NON-NLS-1$ //$NON-NLS-2$
		String msg = "Realm {0}: Using {1} JDBC compliant Driver {2}.{3}"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, connectionInfo.getRealm(), compliant, driver.getMajorVersion(),
				driver.getMinorVersion());
		logger.info(msg);
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
		connectionCheck.checkConnections();

		boolean allowSchemaCreation = componentConfiguration.getBoolean(PROP_ALLOW_SCHEMA_CREATION, Boolean.FALSE);
		boolean allowSchemaDrop = componentConfiguration.getBoolean(PROP_ALLOW_SCHEMA_DROP, Boolean.FALSE);
		boolean allowDataInitOnSchemaCreate = componentConfiguration.getBoolean(PROP_ALLOW_DATA_INIT_ON_SCHEMA_CREATE,
				Boolean.FALSE);

		DbSchemaVersionCheck schemaVersionCheck = new DbSchemaVersionCheck(allowSchemaCreation, allowSchemaDrop);
		schemaVersionCheck.checkSchemaVersion(this.connetionInfoMap);

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
			String url = dbInfo.getUrl();
			String username = dbInfo.getUsername();
			String password = dbInfo.getPassword();
			Connection connection = DriverManager.getConnection(url, username, password);
			connection.setAutoCommit(false);
			return connection;
		} catch (SQLException e) {
			String msg = MessageFormat.format("Failed to get a connection for {0} due to {1}", dbInfo, e.getMessage()); //$NON-NLS-1$
			throw new StrolchPersistenceException(msg, e);
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
