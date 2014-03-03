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

import java.io.IOException;
import java.io.InputStream;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.MessageFormat;
import java.util.Collection;
import java.util.Map;
import java.util.Properties;

import li.strolch.exception.StrolchException;
import li.strolch.persistence.api.DbConnectionInfo;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.StrolchConfigurationException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.dbc.DBC;
import ch.eitchnet.utils.helper.FileHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings(value = "nls")
public class DbSchemaVersionCheck {

	private static final String RESOURCE_DB_VERSION = "/db_version.properties";
	private static final String PROP_DB_VERSION = "db_version";
	private static final String PROP_ALLOW_SCHEMA_CREATION = "allowSchemaCreation";
	private static final String PROP_ALLOW_SCHEMA_DROP = "allowSchemaDrop";

	private static final Logger logger = LoggerFactory.getLogger(DbSchemaVersionCheck.class);
	private Map<String, DbConnectionInfo> connetionInfoMap;
	private boolean allowSchemaCreation;
	private boolean allowSchemaDrop;

	/**
	 * @param connetionInfoMap
	 * @param componentConfiguration
	 */
	public DbSchemaVersionCheck(Map<String, DbConnectionInfo> connetionInfoMap,
			ComponentConfiguration componentConfiguration) {
		this.connetionInfoMap = connetionInfoMap;

		this.allowSchemaCreation = componentConfiguration.getBoolean(PROP_ALLOW_SCHEMA_CREATION, Boolean.FALSE);
		this.allowSchemaDrop = componentConfiguration.getBoolean(PROP_ALLOW_SCHEMA_DROP, Boolean.FALSE);
	}

	public void checkSchemaVersion() {

		Collection<DbConnectionInfo> values = this.connetionInfoMap.values();

		for (DbConnectionInfo connectionInfo : values) {
			String realm = connectionInfo.getRealm();
			String url = connectionInfo.getUrl();
			String username = connectionInfo.getUsername();
			String password = connectionInfo.getPassword();

			logger.info(MessageFormat.format("[{0}] Checking Schema version...", realm));

			try (Connection con = DriverManager.getConnection(url, username, password);
					Statement st = con.createStatement();) {

				String expectedDbVersion = getExpectedDbVersion();

				// first see if we have any schema
				String msg = "select table_schema, table_name, table_type from information_schema.tables where table_name=''{0}'';";
				String checkSchemaExistsSql = MessageFormat.format(msg, PROP_DB_VERSION);
				try (ResultSet rs = st.executeQuery(checkSchemaExistsSql)) {
					if (!rs.next()) {
						createSchema(realm, expectedDbVersion, st);
					} else {
						checkCurrentVersion(realm, st, expectedDbVersion);
					}
				}

			} catch (SQLException e) {
				String msg = "Failed to open DB connection to URL {0} due to: {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, url, e.getMessage());
				throw new StrolchConfigurationException(msg, e);
			}
		}
	}

	private void checkCurrentVersion(String realm, Statement st, String expectedDbVersion) throws SQLException {
		try (ResultSet rs = st.executeQuery("select id, version from db_version order by id desc;")) {
			if (!rs.next()) {
				createSchema(realm, expectedDbVersion, st);
			} else {
				String currentVersion = rs.getString(2);
				if (expectedDbVersion.equals(currentVersion)) {
					String msg = "[{0}] Schema version {1} is the current version. No changes needed.";
					msg = MessageFormat.format(msg, realm, currentVersion);
					logger.info(msg);
				} else {
					String msg = "[{0}] Schema version is not current. Need to upgrade from {1} to {2}";
					msg = MessageFormat.format(msg, realm, currentVersion, expectedDbVersion);
					logger.warn(msg);
					upgradeSchema(realm, expectedDbVersion, st);
				}
			}
		}
	}

	public static String getExpectedDbVersion() {
		Properties dbVersionProps = new Properties();
		try (InputStream stream = DbSchemaVersionCheck.class.getResourceAsStream(RESOURCE_DB_VERSION);) {
			DBC.PRE.assertNotNull(
					MessageFormat.format("Resource file with name {0} does not exist!", RESOURCE_DB_VERSION), stream);
			dbVersionProps.load(stream);
		} catch (IOException e) {
			String msg = "Expected resource file {0} does not exist or is not a valid properties file: {1}";
			msg = MessageFormat.format(msg, RESOURCE_DB_VERSION, e.getMessage());
			throw new StrolchException(msg, e);
		}
		String dbVersion = dbVersionProps.getProperty(PROP_DB_VERSION);
		String msg = "Missing property {0} in resource file {1}";
		DBC.PRE.assertNotEmpty(MessageFormat.format(msg, PROP_DB_VERSION, RESOURCE_DB_VERSION), dbVersion);
		return dbVersion;
	}

	public static String getSql(String dbVersion, String type) {
		String schemaResourceS = MessageFormat.format("/db_schema_{0}_{1}.sql", dbVersion, type);
		try (InputStream stream = DbSchemaVersionCheck.class.getResourceAsStream(schemaResourceS);) {
			DBC.PRE.assertNotNull(
					MessageFormat.format("Schema Resource file with name {0} does not exist!", schemaResourceS), stream);
			return FileHelper.readStreamToString(stream);
		} catch (IOException e) {
			throw new StrolchException("Schema creation resource file is missing or could not be read: "
					+ schemaResourceS, e);
		}
	}

	/**
	 * @param realm
	 * @param st
	 */
	private void createSchema(String realm, String dbVersion, Statement st) {

		if (!this.allowSchemaCreation) {
			String msg = "[{0}] No schema exists, or is not valid. Schema generation is disabled, thus can not continue!";
			msg = MessageFormat.format(msg, realm);
			throw new StrolchConfigurationException(msg);
		}

		logger.info(MessageFormat.format("[{0}] Creating initial schema...", realm));

		String sql = getSql(dbVersion, "initial");
		try {
			st.execute(sql);
		} catch (SQLException e) {
			logger.error("Failed to execute schema creation SQL: \n" + sql);
			throw new StrolchException("Failed to execute schema generation SQL: " + e.getMessage(), e);
		}

		logger.info(MessageFormat.format("[{0}] Successfully created schema for version {1}", realm, dbVersion));
	}

	private void dropSchema(String realm, String dbVersion, Statement st) {

		if (!this.allowSchemaDrop) {
			String msg = "[{0}] Dropping Schema is disabled, but is required to upgrade current schema...";
			msg = MessageFormat.format(msg, realm);
			throw new StrolchConfigurationException(msg);
		}

		logger.info(MessageFormat.format("[{0}] Dropping existing schema...", realm));

		String sql = getSql(dbVersion, "drop");
		try {
			st.execute(sql);
		} catch (SQLException e) {
			logger.error("Failed to execute schema drop SQL: \n" + sql);
			throw new StrolchException("Failed to execute schema drop SQL: " + e.getMessage(), e);
		}
	}

	/**
	 * @param st
	 */
	private void upgradeSchema(String realm, String dbVersion, Statement st) {
		dropSchema(realm, dbVersion, st);
		createSchema(realm, dbVersion, st);
	}

}
