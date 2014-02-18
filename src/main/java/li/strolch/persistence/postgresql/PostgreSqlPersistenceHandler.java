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

import static ch.eitchnet.utils.helper.StringHelper.DOT;

import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.agent.impl.StrolchRealm;
import li.strolch.persistence.api.DbConnectionInfo;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.StrolchConfigurationException;
import li.strolch.runtime.observer.ObserverHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PostgreSqlPersistenceHandler extends StrolchComponent implements PersistenceHandler {

	private static final String PROP_DB_URL = "db.url"; //$NON-NLS-1$
	private static final String PROP_DB_USERNAME = "db.username"; //$NON-NLS-1$
	private static final String PROP_DB_PASSWORD = "db.password"; //$NON-NLS-1$

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

			String dbUrlKey = PROP_DB_URL;
			String dbUsernameKey = PROP_DB_USERNAME;
			String dbPasswordKey = PROP_DB_PASSWORD;
			if (!realmName.equals(StrolchConstants.DEFAULT_REALM)) {
				dbUrlKey += DOT + realmName;
				dbUsernameKey += DOT + realmName;
				dbPasswordKey += DOT + realmName;
			}

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
		String msg = "Using {0} JDBC compliant Driver {1}.{2}"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, compliant, driver.getMajorVersion(), driver.getMinorVersion());
		logger.info(msg);

	}

	@Override
	public void start() {

		// test all connections
		DbConnectionCheck connectionCheck = new DbConnectionCheck(this.connetionInfoMap);
		connectionCheck.checkConnections();

		DbSchemaVersionCheck schemaVersionCheck = new DbSchemaVersionCheck(this.connetionInfoMap,
				this.componentConfiguration);
		schemaVersionCheck.checkSchemaVersion();

		super.start();
	}

	public StrolchTransaction openTx(StrolchRealm realm) {
		PostgreSqlStrolchTransaction tx = new PostgreSqlStrolchTransaction(realm, this);
		if (getContainer().hasComponent(ObserverHandler.class)) {
			tx.setObserverHandler(getContainer().getComponent(ObserverHandler.class));
		}
		return tx;
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
}
