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

import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;

import li.strolch.persistence.api.DbConnectionInfo;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchPersistenceHandler;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.component.ComponentContainer;
import li.strolch.runtime.component.StrolchComponent;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.StrolchConfigurationException;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PostgreSqlPersistenceHandler extends StrolchComponent implements StrolchPersistenceHandler {

	private static final String PROP_DB_URL = "db.url";
	private static final String PROP_DB_USERNAME = "db.username";
	private static final String PROP_DB_PASSWORD = "db.password";

	private ComponentConfiguration componentConfiguration;
	private Map<String, DbConnectionInfo> connetionInfoMap;

	public PostgreSqlPersistenceHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration componentConfiguration) {

		this.componentConfiguration = componentConfiguration;
		this.connetionInfoMap = new HashMap<>();

		String dbUrl = componentConfiguration.getString(PROP_DB_URL, null);
		String username = componentConfiguration.getString(PROP_DB_USERNAME, null);
		String password = componentConfiguration.getString(PROP_DB_PASSWORD, null);

		Driver driver;
		try {
			driver = DriverManager.getDriver(dbUrl);
		} catch (SQLException e) {
			String msg = "Failed to load DB driver for URL {0} due to: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, dbUrl, e.getMessage());
			throw new StrolchConfigurationException(msg, e);
		}

		DbConnectionInfo connectionInfo = new DbConnectionInfo(StrolchTransaction.DEFAULT_REALM, dbUrl);
		connectionInfo.setUsername(username);
		connectionInfo.setPassword(password);
		this.connetionInfoMap.put(StrolchTransaction.DEFAULT_REALM, connectionInfo);

		String compliant = driver.jdbcCompliant() ? "" : "non"; //$NON-NLS-1$ //$NON-NLS-2$
		String msg = "Using {0} JDBC compliant Driver {1}.{2}"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, compliant, driver.getMajorVersion(), driver.getMinorVersion());
		logger.info(msg);

		super.initialize(componentConfiguration);
	}

	@Override
	public void start() {

		// test all connections
		DbConnectionCheck connectionCheck = new DbConnectionCheck(this.connetionInfoMap);
		connectionCheck.checkConnections();

		DbSchemaVersionCheck schemaVersionCheck = new DbSchemaVersionCheck(this.connetionInfoMap,
				componentConfiguration);
		schemaVersionCheck.checkSchemaVersion();

		super.start();
	}

	public StrolchTransaction openTx() {
		return openTx(StrolchTransaction.DEFAULT_REALM);
	}

	@SuppressWarnings("resource")
	// caller will/must close
	public StrolchTransaction openTx(String realm) {
//		PersistenceTransaction tx = this.persistenceManager.openTx(realm);
//		XmlStrolchTransaction strolchTx = new XmlStrolchTransaction(tx);
//		if (getContainer().hasComponent(ObserverHandler.class)) {
//			strolchTx.setObserverHandler(getContainer().getComponent(ObserverHandler.class));
//		}
//		return strolchTx;
		return null;
	}

	@Override
	public OrderDao getOrderDao(StrolchTransaction tx) {
		return new PostgreSqlOrderDao(tx);
	}

	@Override
	public ResourceDao getResourceDao(StrolchTransaction tx) {
		return new PostgreSqlResourceDao(tx);
	}
}
