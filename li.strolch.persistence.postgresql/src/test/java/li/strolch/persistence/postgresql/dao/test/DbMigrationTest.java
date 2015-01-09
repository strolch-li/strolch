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

import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.DB_PASSWORD;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.DB_URL;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.DB_USERNAME;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.dropSchema;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.text.MessageFormat;

import li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler;
import li.strolch.runtime.StrolchConstants;

import org.junit.BeforeClass;
import org.junit.Test;

import ch.eitchnet.db.DbException;
import ch.eitchnet.db.DbSchemaVersionCheck;
import ch.eitchnet.utils.Version;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DbMigrationTest {

	@BeforeClass
	public static void beforeClass() throws Exception {
		dropSchema(DB_URL, DB_USERNAME, DB_PASSWORD);
	}

	@Test
	public void shouldMigrate() throws Exception {

		String scriptPrefix = PostgreSqlPersistenceHandler.SCRIPT_PREFIX;
		Class<?> ctxClass = PostgreSqlPersistenceHandler.class;
		boolean allowSchemaCreation = true;
		boolean allowSchemaMigration = true;
		boolean allowSchemaDrop = true;
		DbSchemaVersionCheck dbCheck = new DbSchemaVersionCheck(scriptPrefix, ctxClass, allowSchemaCreation,
				allowSchemaMigration, allowSchemaDrop);

		try (Connection con = DriverManager.getConnection(DB_URL, DB_USERNAME, DB_PASSWORD)) {

			// DROP 0.2.1
			dbCheck.dropSchema(con, StrolchConstants.DEFAULT_REALM, Version.valueOf("0.2.1"));

			// CREATE 0.2.0
			dbCheck.createSchema(con, StrolchConstants.DEFAULT_REALM, Version.valueOf("0.2.0"));

			// MIGRATE 0.2.1
			dbCheck.migrateSchema(con, StrolchConstants.DEFAULT_REALM, Version.valueOf("0.2.1"));

		} catch (SQLException e) {
			String msg = "Failed to open DB connection to URL {0} due to: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, DB_URL, e.getMessage());
			throw new DbException(msg, e);
		}
	}
}
