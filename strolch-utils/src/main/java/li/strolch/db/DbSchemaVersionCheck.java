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
package li.strolch.db;

import static li.strolch.db.DbConstants.PROP_DB_VERSION;
import static li.strolch.db.DbConstants.RESOURCE_DB_VERSION;

import javax.sql.DataSource;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.file.Paths;
import java.security.CodeSource;
import java.sql.*;
import java.text.MessageFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import li.strolch.utils.Version;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.ExceptionHelper;
import li.strolch.utils.helper.FileHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class DbSchemaVersionCheck {

	private static final Logger logger = LoggerFactory.getLogger(DbSchemaVersionCheck.class);
	private final String app;
	private final Class<?> ctxClass;
	private final boolean allowSchemaCreation;
	private final boolean allowSchemaMigration;
	private final boolean allowSchemaDrop;
	private final Map<String, DbMigrationState> dbMigrationStates;

	/**
	 * @param app
	 * 		the app name
	 * @param ctxClass
	 * 		the context class
	 * @param allowSchemaCreation
	 * 		true allows creating the schema
	 * @param allowSchemaDrop
	 * 		true allows dropping the schema
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

	public void checkSchemaVersion(Map<String, DataSource> dsMap) throws DbException {
		for (Entry<String, DataSource> entry : dsMap.entrySet()) {
			String realm = entry.getKey();
			DataSource ds = entry.getValue();
			DbMigrationState dbMigrationState = checkSchemaVersion(realm, ds);
			this.dbMigrationStates.put(realm, dbMigrationState);
		}
	}

	/**
	 * Checks the state of the realm's DB schema
	 *
	 * @param realm
	 * 		the realm
	 * @param ds
	 * 		the {@link DataSource}
	 *
	 * @return the state
	 *
	 * @throws DbException
	 * 		if something goes wrong
	 */
	public DbMigrationState checkSchemaVersion(String realm, DataSource ds) throws DbException {

		logger.info("[{}:{}] Checking Schema version for: {}", this.app, realm, ds);

		Version expectedDbVersion = getExpectedDbVersion(this.app, this.ctxClass);

		// get current version
		Version currentVersion;
		try (Connection con = ds.getConnection()) {
			currentVersion = getCurrentVersion(con, this.app);
			con.commit();
		} catch (SQLException e) {
			String msg = "Failed to open DB connection to {0} due to: {1}";
			msg = MessageFormat.format(msg, ds, e.getMessage());
			throw new DbException(msg, e);
		}

		try (Connection con = ds.getConnection()) {
			DbMigrationState migrationType = detectMigrationState(realm, expectedDbVersion, currentVersion);

			switch (migrationType) {
			case CREATED -> createSchema(con, realm, expectedDbVersion);
			case MIGRATED -> migrateSchema(con, realm, currentVersion, expectedDbVersion);
			case DROPPED_CREATED -> throw new DbException("Migration type " + migrationType + " not handled!");

			// do nothing
			case NOTHING -> {
			}
			}

			con.commit();
			return migrationType;

		} catch (SQLException e) {
			String msg = "Failed to open DB connection to {0} due to: {1}";
			msg = MessageFormat.format(msg, ds, e.getMessage());
			throw new DbException(msg, e);
		}
	}

	/**
	 * @param con
	 * 		the connection
	 * @param app
	 * 		the app for which to get the version
	 *
	 * @return the version
	 *
	 * @throws SQLException
	 * 		if something goes wrong
	 */
	public static Version getCurrentVersion(Connection con, String app) throws SQLException {

		// first find current version
		String sql = "select id, version from db_version where app = ? order by id desc;";
		Version currentVersion = null;
		try (PreparedStatement st = con.prepareStatement(sql)) {
			st.setString(1, app);
			try (ResultSet rs = st.executeQuery()) {
				if (rs.next())
					currentVersion = Version.valueOf(rs.getString(2));
			}
		} catch (Exception e) {
			logger.error("Failed to query version for app {}: {}", app, ExceptionHelper.formatExceptionMessage(e));
			return null;
		}

		return currentVersion;
	}

	/**
	 * @param realm
	 * 		the realm
	 * @param expectedDbVersion
	 * 		the expected version
	 *
	 * @return the migration state
	 *
	 * @throws DbException
	 * 		if something goes wrong
	 */
	public DbMigrationState detectMigrationState(String realm, Version expectedDbVersion, Version currentVersion)
			throws DbException {

		// no version, then we need to create it
		if (currentVersion == null)
			return DbMigrationState.CREATED;

		// otherwise parse the version
		int compare = expectedDbVersion.compareTo(currentVersion);
		if (compare == 0) {
			String msg = "[{0}:{1}] Schema version {2} is the current version. No changes needed.";
			msg = MessageFormat.format(msg, this.app, realm, currentVersion);
			logger.info(msg);
			return DbMigrationState.NOTHING;
		} else if (compare > 0) {
			String msg = "[{0}:{1}] Schema version is not current. Need to upgrade from {2} to {3}";
			msg = MessageFormat.format(msg, this.app, realm, currentVersion, expectedDbVersion);
			logger.warn(msg);
			return DbMigrationState.MIGRATED;
		}

		throw new DbException(
				MessageFormat.format("[{0}:{1}]Current version {2} is later than expected version {3}", this.app, realm,
						currentVersion, expectedDbVersion));
	}

	/**
	 * @param app
	 * 		the app
	 * @param ctxClass
	 * 		the context class
	 *
	 * @return the version
	 *
	 * @throws DbException
	 * 		if something goes wrong
	 */
	public static Version getExpectedDbVersion(String app, Class<?> ctxClass) throws DbException {
		Properties dbVersionProps = new Properties();

		String dbVersionPropFile = MessageFormat.format(RESOURCE_DB_VERSION, app);

		try (InputStream stream = ctxClass.getResourceAsStream(dbVersionPropFile)) {
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
	 * 		script file prefix
	 * @param ctxClass
	 * 		the class to get the class loader to use to load the resource
	 * @param version
	 * 		the version to load
	 * @param type
	 * 		the operation type
	 *
	 * @return the SQL to perform
	 *
	 * @throws DbException
	 * 		if something goes wrong
	 */
	public static String getSql(String scriptPrefix, Class<?> ctxClass, Version version, String type)
			throws DbException {
		String schemaResourceS = MessageFormat.format("/{0}_db_schema_{1}_{2}.sql", scriptPrefix, version, type);
		try (InputStream stream = ctxClass.getResourceAsStream(schemaResourceS)) {

			DBC.PRE.assertNotNull(
					MessageFormat.format("Schema Resource file with name {0} does not exist!", schemaResourceS),
					stream);

			return FileHelper.readStreamToString(stream);

		} catch (IOException e) {
			throw new DbException("Schema creation resource file is missing or could not be read: " + schemaResourceS,
					e);
		}
	}

	/**
	 * @param realm
	 * 		the realm to create the schema for (a {@link DataSource} must exist for it)
	 * @param version
	 * 		the version to upgrade to
	 *
	 * @throws DbException
	 * 		if something goes wrong
	 */
	public void createSchema(Connection con, String realm, Version version) throws DbException {

		if (!this.allowSchemaCreation) {
			String msg = "[{0}:{1}] No schema exists, or is not valid. Schema generation is disabled, thus can not continue!";
			msg = MessageFormat.format(msg, this.app, realm);
			throw new DbException(msg);
		}

		logger.info("[{}:{}] Creating initial schema version {}...", this.app, realm, version);

		String sql = getSql(this.app, this.ctxClass, version, "initial");

		try (Statement st = con.createStatement()) {
			st.execute(sql);
		} catch (SQLException e) {
			logger.error("Failed to execute schema creation SQL: \n{}", sql);
			throw new DbException("Failed to execute schema generation SQL: " + e.getMessage(), e);
		}

		logger.info("[{}:{}] Successfully created schema with version {}", this.app, realm, version);
	}

	/**
	 * Upgrades the schema to the given version. If the current version is below the given version, then currently this
	 * method drops the schema and recreates it. Real migration must still be implemented
	 *
	 * @param realm
	 * 		the realm to migrate (a {@link DataSource} must exist for it)
	 * @param expectedVersion
	 * 		the version to upgrade to
	 *
	 * @throws DbException
	 * 		if something goes wrong
	 */
	public void migrateSchema(Connection con, String realm, Version currentVersion, Version expectedVersion)
			throws DbException {

		if (!this.allowSchemaMigration) {
			String msg = "[{0}:{1}] Schema is not valid. Schema migration is disabled, thus can not continue!";
			msg = MessageFormat.format(msg, this.app, realm);
			throw new DbException(msg);
		}

		if (expectedVersion.equals(currentVersion))
			throw new IllegalStateException("Expected version " + expectedVersion + " is same as " + currentVersion
					+ " and thus no migration is necessary!");
		if (expectedVersion.compareTo(currentVersion) < 0)
			throw new IllegalStateException(
					"Expected version " + expectedVersion + " is weirdly before current version" + currentVersion
							+ " for " + this.app);

		logger.info("[{}:{}] Migrating schema from {} to {}...", this.app, realm, currentVersion, expectedVersion);

		// first get all possible migration scripts
		List<Version> versions = parseMigrationVersions();
		if (versions.isEmpty())
			throw new IllegalStateException("No migration versions found for context " + this.app);
		versions.sort(Version::compareTo);

		if (!versions.contains(expectedVersion))
			throw new IllegalStateException(
					"Expected version " + expectedVersion + " is missing as a migration version for " + this.app);

		for (Version version : versions) {
			if (version.compareTo(currentVersion) <= 0)
				continue;

			logger.info("Migrating to version {}...", version);

			String sql = getSql(this.app, this.ctxClass, version, "migration");
			try (Statement st = con.createStatement()) {
				st.execute(sql);
			} catch (SQLException e) {
				logger.error("Failed to execute schema migration SQL: \n{}", sql);
				throw new DbException("Failed to execute schema migration SQL: " + e.getMessage(), e);
			}
		}

		try {
			Version version = getCurrentVersion(con, this.app);
			if (version == null || !version.equals(expectedVersion))
				throw new IllegalStateException(
						"Migration to version " + expectedVersion + " failed as version after migration is " + version);
		} catch (SQLException e) {
			throw new IllegalStateException("Failed to read current version", e);
		}

		logger.info("[{}:{}] Successfully migrated schema to version {}", this.app, realm, expectedVersion);
	}

	public List<Version> parseMigrationVersions() {

		List<Version> versions = new ArrayList<>();

		try {
			CodeSource src = this.ctxClass.getProtectionDomain().getCodeSource();
			URL url = src.getLocation();

			String scheme = url.toURI().getScheme();
			if (scheme.equals("jar") || scheme.equals("file") && url.toString().endsWith(".jar")) {
				try (ZipInputStream zip = new ZipInputStream(url.openStream())) {
					ZipEntry ze;
					while ((ze = zip.getNextEntry()) != null) {
						String entryName = ze.getName();
						if (entryName.endsWith(".sql") && entryName.startsWith(this.app) && entryName.contains(
								"migration"))
							versions.add(parseVersion(entryName));
					}
				} catch (IOException e) {
					throw new IllegalStateException("Failed to read JAR: " + url, e);
				}

			} else if (scheme.equals("file")) {

				File file = Paths.get(url.toURI()).toFile();
				File[] files = file.listFiles();
				if (files != null) {
					for (File f : files) {
						String name = f.getName();
						if (name.endsWith(".sql") && name.startsWith(this.app) && name.contains("migration"))
							versions.add(parseVersion(name));
					}
				}
			}

		} catch (Exception e) {
			throw new IllegalStateException("Failed to parse migration script versions", e);
		}

		return versions;
	}

	private Version parseVersion(String scriptName) {
		int versionStart = (this.app + "_db_schema_").length();
		int versionEnd = scriptName.indexOf("_", versionStart);
		return Version.valueOf(scriptName.substring(versionStart, versionEnd));
	}

	/**
	 * @param realm
	 * 		the realm for which the schema must be dropped (a {@link DataSource} must exist for it)
	 * @param version
	 * 		the version with which to to drop the schema
	 *
	 * @throws DbException
	 * 		if something goes wrong
	 */
	public void dropSchema(Connection con, String realm, Version version) throws DbException {

		if (!this.allowSchemaDrop) {
			String msg = "[{0}:{1}] Dropping Schema is disabled, but is required to upgrade current schema...";
			msg = MessageFormat.format(msg, this.app, realm);
			throw new DbException(msg);
		}

		logger.info("[{}:{}] Dropping existing schema version {}...", this.app, realm, version);

		String sql = getSql(this.app, this.ctxClass, version, "drop");
		try (Statement st = con.createStatement()) {
			st.execute(sql);
		} catch (SQLException e) {
			logger.error("Failed to execute schema drop SQL: \n{}", sql);
			throw new DbException("Failed to execute schema drop SQL: " + e.getMessage(), e);
		}

		logger.info("[{}:{}] Successfully dropped schema with version {}", this.app, realm, version);
	}
}
