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
package li.strolch.persistence.postgresql.dao.test;

import static li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler.SCRIPT_PREFIX_ARCHIVE;
import static li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler.SCRIPT_PREFIX_STROLCH;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.*;
import static li.strolch.runtime.StrolchConstants.DEFAULT_REALM;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.text.MessageFormat;

import li.strolch.db.DbException;
import li.strolch.db.DbSchemaVersionCheck;
import li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler;
import li.strolch.utils.Version;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DbSchemaMigrationTest {

	private static final Logger logger = LoggerFactory.getLogger(DbSchemaMigrationTest.class);

	@BeforeClass
	public static void beforeClass() throws Exception {
		dropSchema(SCRIPT_PREFIX_ARCHIVE, DB_URL, DB_USERNAME, DB_PASSWORD);
		dropSchema(SCRIPT_PREFIX_STROLCH, DB_URL, DB_USERNAME, DB_PASSWORD);
	}

	@Test
	public void shouldMigrate() throws Exception {
		migrate(SCRIPT_PREFIX_STROLCH);
		migrate(SCRIPT_PREFIX_ARCHIVE);
	}

	private void migrate(String scriptPrefix) throws Exception {

		logger.info("");
		logger.info("===============================================");
		logger.info("Trying to migrate DB schema from 0.1.0 upwards...");
		logger.info("");

		// first clear DB
		dropSchema(scriptPrefix, DB_URL, DB_USERNAME, DB_PASSWORD);

		DbSchemaVersionCheck dbCheck = new DbSchemaVersionCheck(scriptPrefix, PostgreSqlPersistenceHandler.class, true,
				true, true);

		try (Connection con = DriverManager.getConnection(DB_URL, DB_USERNAME, DB_PASSWORD)) {

			// CREATE 0.1.0
			Version currentVersion = Version.valueOf("0.1.0");
			dbCheck.createSchema(con, DEFAULT_REALM, currentVersion);

			Version expectedDbVersion = DbSchemaVersionCheck
					.getExpectedDbVersion(scriptPrefix, PostgreSqlPersistenceHandler.class);

			// MIGRATE
			dbCheck.migrateSchema(con, DEFAULT_REALM, currentVersion, expectedDbVersion);

		} catch (SQLException e) {
			String msg = "Failed to open DB connection to URL {0} due to: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, DB_URL, e.getMessage());
			throw new DbException(msg, e);
		}
	}
}
