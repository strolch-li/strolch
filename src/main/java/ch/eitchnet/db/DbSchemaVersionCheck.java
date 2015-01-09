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
package ch.eitchnet.db;

import static ch.eitchnet.db.DbConstants.PROP_DB_VERSION;
import static ch.eitchnet.db.DbConstants.RESOURCE_DB_VERSION;

import java.io.IOException;
import java.io.InputStream;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.Version;
import ch.eitchnet.utils.dbc.DBC;
import ch.eitchnet.utils.helper.FileHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings(value = "nls")
public class DbSchemaVersionCheck {

	private static final Logger logger = LoggerFactory.getLogger(DbSchemaVersionCheck.class);
	private String app;
	private Class<?> ctxClass;
	private boolean allowSchemaCreation;
	private boolean allowSchemaMigration;
	private boolean allowSchemaDrop;
	private Map<String, DbMigrationState> dbMigrationStates;

	/**
	 * @param app
	 * @param ctxClass
	 * @param allowSchemaCreation
	 * @param allowSchemaDrop
	 */
	public DbSchemaVersionCheck(String app, Class<?> ctxClass, boolean allowSchemaCreation,
			boolean allowSchemaMigration, boolean allowSchemaDrop) {

		DBC.PRE.assertNotEmpty("app may not be empty!", app);
		DBC.PRE.assertNotNull("ctxClass may not be null!", ctxClass);

		this.app = app;
		this.ctxClass = ctxClass;
		this.allowSchemaCreation = allowSchemaCreation;
		this.allowSchemaMigration = allowSchemaMigration;
		this.allowSchemaDrop = allowSchemaDrop;
		this.dbMigrationStates = new HashMap<>();
	}

	/**
	 * @return the dbMigrationStates
	 */
	public Map<String, DbMigrationState> getDbMigrationStates() {
		return this.dbMigrationStates;
	}

	public void checkSchemaVersion(Map<String, DbConnectionInfo> connectionInfoMap) throws DbException {
		for (DbConnectionInfo connectionInfo : connectionInfoMap.values()) {
			DbMigrationState dbMigrationState = checkSchemaVersion(connectionInfo);
			dbMigrationStates.put(connectionInfo.getRealm(), dbMigrationState);
		}
	}

	/**
	 * Returns true if the schema existed or was only migrated, false if the schema was created
	 * 
	 * @param connectionInfo
	 * 
	 * @return true if the schema existed or was only migrated, false if the schema was created
	 * 
	 * @throws DbException
	 */
	public DbMigrationState checkSchemaVersion(DbConnectionInfo connectionInfo) throws DbException {
		String realm = connectionInfo.getRealm();
		String url = connectionInfo.getUrl();
		String username = connectionInfo.getUsername();
		String password = connectionInfo.getPassword();

		logger.info(MessageFormat.format("[{0}] Checking Schema version for: {1}@{2}", realm, username, url));

		Version expectedDbVersion = getExpectedDbVersion(this.app, this.ctxClass);

		try (Connection con = DriverManager.getConnection(url, username, password)) {

			// get current version
			Version currentVersion = getCurrentVersion(con, this.app);
			DbMigrationState migrationType = detectMigrationState(realm, expectedDbVersion, currentVersion);

			switch (migrationType) {
			case CREATED:
				createSchema(con, realm, expectedDbVersion);
				break;
			case MIGRATED:
				migrateSchema(con, realm, expectedDbVersion);
				break;
			case DROPPED_CREATED:
				throw new DbException("Migration type " + migrationType + " not handled!");
			case NOTHING:
				// do nothing
			default:
				break;
			}

			return migrationType;

		} catch (SQLException e) {
			String msg = "Failed to open DB connection to URL {0} due to: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, url, e.getMessage());
			throw new DbException(msg, e);
		}
	}

	/**
	 * @param con
	 * @param app
	 * 
	 * @return
	 * 
	 * @throws SQLException
	 */
	public static Version getCurrentVersion(Connection con, String app) throws SQLException {

		// first see if we have any schema
		String sql = "select table_schema, table_name, table_type from information_schema.tables where table_name = ?";
		try (PreparedStatement st = con.prepareStatement(sql)) {
			st.setString(0, PROP_DB_VERSION);
			if (!st.executeQuery().next())
				return null;
		}

		// first find current version
		sql = "select id, version from db_version where app = ? order by id desc;";
		Version currentVersion = null;
		try (PreparedStatement st = con.prepareStatement(sql)) {
			st.setString(0, app);
			ResultSet rs = st.executeQuery();
			if (rs.next())
				currentVersion = Version.valueOf(rs.getString(2));
		}

		return currentVersion;
	}

	/**
	 * @param realm
	 * @param expectedDbVersion
	 * 
	 * @return
	 * 
	 * @throws SQLException
	 * @throws DbException
	 */
	public DbMigrationState detectMigrationState(String realm, Version expectedDbVersion, Version currentVersion)
			throws SQLException, DbException {

		// no version, then we need to create it
		if (currentVersion == null)
			return DbMigrationState.CREATED;

		// otherwise parse the version
		int compare = expectedDbVersion.compareTo(currentVersion);
		if (compare == 0) {
			String msg = "[{0}] Schema version {1} is the current version. No changes needed.";
			msg = MessageFormat.format(msg, realm, currentVersion);
			logger.info(msg);
			return DbMigrationState.NOTHING;
		} else if (compare > 0) {
			String msg = "[{0}] Schema version is not current. Need to upgrade from {1} to {2}";
			msg = MessageFormat.format(msg, realm, currentVersion, expectedDbVersion);
			logger.warn(msg);
			return DbMigrationState.MIGRATED;
		}

		throw new DbException("Current version " + currentVersion + " is later than expected version "
				+ expectedDbVersion);
	}

	/**
	 * @param app
	 * @param ctxClass
	 * 
	 * @return
	 * 
	 * @throws DbException
	 */
	public static Version getExpectedDbVersion(String app, Class<?> ctxClass) throws DbException {
		Properties dbVersionProps = new Properties();

		String dbVersionPropFile = MessageFormat.format(RESOURCE_DB_VERSION, app);

		try (InputStream stream = ctxClass.getResourceAsStream(dbVersionPropFile);) {
			DBC.PRE.assertNotNull(
					MessageFormat.format("Resource file with name {0} does not exist!", dbVersionPropFile), stream);
			dbVersionProps.load(stream);
		} catch (IOException e) {
			String msg = "Expected resource file {0} does not exist or is not a valid properties file: {1}";
			msg = MessageFormat.format(msg, dbVersionPropFile, e.getMessage());
			throw new DbException(msg, e);
		}
		String dbVersion = dbVersionProps.getProperty(PROP_DB_VERSION);
		String msg = "Missing property {0} in resource file {1}";
		DBC.PRE.assertNotEmpty(MessageFormat.format(msg, PROP_DB_VERSION, dbVersionPropFile), dbVersion);

		return Version.valueOf(dbVersion);
	}

	/**
	 * @param scriptPrefix
	 * @param classLoader
	 * @param dbVersion
	 * @param type
	 * 
	 * @return
	 * 
	 * @throws DbException
	 */
	public static String getSql(String scriptPrefix, Class<?> ctxClass, Version dbVersion, String type)
			throws DbException {
		String schemaResourceS = MessageFormat.format("/{0}_db_schema_{1}_{2}.sql", scriptPrefix, dbVersion, type);
		try (InputStream stream = ctxClass.getResourceAsStream(schemaResourceS);) {
			DBC.PRE.assertNotNull(
					MessageFormat.format("Schema Resource file with name {0} does not exist!", schemaResourceS), stream);
			return FileHelper.readStreamToString(stream);
		} catch (IOException e) {
			throw new DbException("Schema creation resource file is missing or could not be read: " + schemaResourceS,
					e);
		}
	}

	/**
	 * 
	 * @param realm
	 *            the realm to create the schema for (a {@link DbConnectionInfo} must exist for it)
	 * @param dbVersion
	 *            the version to upgrade to
	 * @param st
	 *            the open database {@link Statement} to which the SQL statements will be written
	 * 
	 * @return true if the schema was created, false if it was not
	 * 
	 * @throws DbException
	 */
	public DbMigrationState createSchema(Connection con, String realm, Version dbVersion) throws DbException {

		if (!this.allowSchemaCreation) {
			String msg = "[{0}] No schema exists, or is not valid. Schema generation is disabled, thus can not continue!";
			msg = MessageFormat.format(msg, realm);
			throw new DbException(msg);
		}

		logger.info(MessageFormat.format("[{0}] Creating initial schema...", realm));

		String sql = getSql(this.app, this.ctxClass, dbVersion, "initial");

		try (Statement st = con.createStatement()) {
			st.execute(sql);
		} catch (SQLException e) {
			logger.error("Failed to execute schema creation SQL: \n" + sql);
			throw new DbException("Failed to execute schema generation SQL: " + e.getMessage(), e);
		}

		logger.info(MessageFormat.format("[{0}] Successfully created schema for version {1}", realm, dbVersion));
		return DbMigrationState.CREATED;
	}

	/**
	 * Upgrades the schema to the given version. If the current version is below the given version, then currently this
	 * method drops the schema and recreates it. Real migration must still be implemented
	 * 
	 * @param realm
	 *            the realm to migrate (a {@link DbConnectionInfo} must exist for it)
	 * @param dbVersion
	 *            the version to upgrade to
	 * 
	 * @return true if the schema was recreated, false if it was simply migrated
	 * 
	 * @throws DbException
	 */
	public DbMigrationState migrateSchema(Connection con, String realm, Version dbVersion) throws DbException {

		if (!this.allowSchemaMigration) {
			String msg = "[{0}] Schema is not valid. Schema migration is disabled, thus can not continue!";
			msg = MessageFormat.format(msg, realm);
			throw new DbException(msg);
		}

		logger.info(MessageFormat.format("[{0}] Migrating schema...", realm));

		String sql = getSql(this.app, this.ctxClass, dbVersion, "migration");
		try (Statement st = con.createStatement()) {
			st.execute(sql);
		} catch (SQLException e) {
			logger.error("Failed to execute schema migration SQL: \n" + sql);
			throw new DbException("Failed to execute schema migration SQL: " + e.getMessage(), e);
		}

		logger.info(MessageFormat.format("[{0}] Successfully migrated schema to version {1}", realm, dbVersion));
		return DbMigrationState.MIGRATED;
	}

	/**
	 * @param realm
	 *            the realm for which the schema must be dropped (a {@link DbConnectionInfo} must exist for it)
	 * @param dbVersion
	 *            the version with which to to drop the schema
	 * @param st
	 *            the open database {@link Statement} to which the SQL statements will be written
	 * 
	 * @throws DbException
	 */
	public void dropSchema(Connection con, String realm, Version dbVersion) throws DbException {

		if (!this.allowSchemaDrop) {
			String msg = "[{0}] Dropping Schema is disabled, but is required to upgrade current schema...";
			msg = MessageFormat.format(msg, realm);
			throw new DbException(msg);
		}

		logger.info(MessageFormat.format("[{0}] Dropping existing schema...", realm));

		String sql = getSql(this.app, this.ctxClass, dbVersion, "drop");
		try (Statement st = con.createStatement()) {
			st.execute(sql);
		} catch (SQLException e) {
			logger.error("Failed to execute schema drop SQL: \n" + sql);
			throw new DbException("Failed to execute schema drop SQL: " + e.getMessage(), e);
		}
	}
}
