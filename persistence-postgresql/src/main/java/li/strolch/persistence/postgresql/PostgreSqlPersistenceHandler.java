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

import static li.strolch.db.DbConstants.*;

import javax.sql.DataSource;
import java.sql.Connection;
import java.text.MessageFormat;
import java.util.Map;
import java.util.Map.Entry;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.db.DbMigrationState;
import li.strolch.db.DbSchemaVersionCheck;
import li.strolch.persistence.api.*;
import li.strolch.persistence.postgresql.PostgreSqlDbConnectionBuilder.StrolchPostgreDataSource;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.DbConnectionBuilder;
import li.strolch.runtime.privilege.PrivilegeHandler;
import org.postgresql.Driver;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PostgreSqlPersistenceHandler extends StrolchComponent implements PersistenceHandler {

	public static final String SCRIPT_PREFIX_STROLCH = "strolch";
	public static final String SCRIPT_PREFIX_ARCHIVE = "archive";
	public static final String PROP_DATA_TYPE = "dataType";
	public static final String DATA_TYPE_XML = "xml";
	public static final String DATA_TYPE_JSON = "json";

	private Map<String, DataSource> dsMap;
	private DataType dataType;

	public PostgreSqlPersistenceHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public boolean supportsPaging() {
		return true;
	}

	public DataType getDataType() {
		return this.dataType;
	}

	@Override
	public void initialize(ComponentConfiguration componentConfiguration) throws Exception {

		if (!Driver.isRegistered())
			Driver.register();

		DbConnectionBuilder connectionBuilder = new PostgreSqlDbConnectionBuilder(getContainer(),
				componentConfiguration);
		this.dsMap = connectionBuilder.build();
		this.dataType = DataType.valueOf(componentConfiguration.getString(PROP_DATA_TYPE, DATA_TYPE_XML).toLowerCase());
		super.initialize(componentConfiguration);
	}

	@Override
	public ComponentConfiguration getConfiguration() {
		return super.getConfiguration();
	}

	/**
	 * Returns the map of {@link DataSource} which can be used in maintenance mode
	 *
	 * @return the dsMap
	 */
	public Map<String, DataSource> getDataSources() {
		return this.dsMap;
	}

	@Override
	public void start() throws Exception {

		ComponentConfiguration config = getConfiguration();
		boolean allowSchemaCreation = config.getBoolean(PROP_ALLOW_SCHEMA_CREATION, false);
		boolean allowSchemaMigration = config.getBoolean(PROP_ALLOW_SCHEMA_MIGRATION, false);
		boolean allowSchemaDrop = config.getBoolean(PROP_ALLOW_SCHEMA_DROP, false);
		boolean allowDataInitOnSchemaCreate = config.getBoolean(PROP_ALLOW_DATA_INIT_ON_SCHEMA_CREATE, false);

		DbSchemaVersionCheck schemaVersionCheck = new DbSchemaVersionCheck(SCRIPT_PREFIX_STROLCH, this.getClass(),
				allowSchemaCreation, allowSchemaMigration, allowSchemaDrop);
		schemaVersionCheck.checkSchemaVersion(this.dsMap);

		// if allowed, perform DB initialization
		if (!allowDataInitOnSchemaCreate) {
			logger.info("Data Initialization not enabled as 'allowDataInitOnSchemaCreate' is false!");
		} else {
			Map<String, DbMigrationState> dbMigrationStates = schemaVersionCheck.getDbMigrationStates();
			String msg = "Data Initialization is enabled, checking for {0} realms if DB initialization is required...";
			logger.info(MessageFormat.format(msg, dbMigrationStates.size()));
			PrivilegeHandler privilegeHandler = getContainer().getPrivilegeHandler();
			StrolchAgent agent = getContainer().getAgent();
			PostgreSqlSchemaInitializer schemaInitializer = new PostgreSqlSchemaInitializer(agent, this,
					dbMigrationStates);
			privilegeHandler.runAsAgent(schemaInitializer);
		}

		super.start();
	}

	@Override
	public void destroy() throws Exception {
		if (this.dsMap != null) {
			for (Entry<String, DataSource> entry : this.dsMap.entrySet()) {
				StrolchPostgreDataSource ds = (StrolchPostgreDataSource) entry.getValue();
				ds.shutdown();
			}
		}

		if (Driver.isRegistered())
			Driver.deregister();

		super.destroy();
	}

	@Override
	public StrolchTransaction openTx(StrolchRealm realm, Certificate certificate, String action, boolean readOnly) {
		return new PostgreSqlStrolchTransaction(getContainer(), realm, certificate, action, readOnly, this);
	}

	public Connection getConnection(String realm) {
		DataSource ds = this.dsMap.get(realm);
		if (ds == null) {
			String msg = MessageFormat.format("There is no DataSource registered for the realm {0}",
					realm);
			throw new StrolchPersistenceException(msg);
		}

		try {
			return ds.getConnection();
		} catch (Exception e) {
			String msg = "Failed to open a connection to {0} due to {1}";
			throw new StrolchPersistenceException(MessageFormat.format(msg, ds, e.getMessage()), e);
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
	public ActivityDao getActivityDao(StrolchTransaction tx) {
		return ((PostgreSqlStrolchTransaction) tx).getActivityDao();
	}

	@Override
	public AuditDao getAuditDao(StrolchTransaction tx) {
		return ((PostgreSqlStrolchTransaction) tx).getAuditDao();
	}

	@Override
	public LogMessageDao getLogMessageDao(StrolchTransaction tx) {
		return ((PostgreSqlStrolchTransaction) tx).getLogMessageDao();
	}
}
