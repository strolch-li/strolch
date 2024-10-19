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

import static li.strolch.db.DbConstants.PROP_DB_HOST_OVERRIDE;
import static li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler.SCRIPT_PREFIX_ARCHIVE;
import static li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler.SCRIPT_PREFIX_STROLCH;
import static li.strolch.runtime.configuration.DbConnectionBuilder.overridePostgresqlHost;
import static org.junit.Assert.assertEquals;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.text.MessageFormat;

import li.strolch.db.DbSchemaVersionCheck;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.postgresql.DataType;
import li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler;
import li.strolch.testbase.runtime.AbstractModelTest;
import li.strolch.testbase.runtime.RuntimeMock;
import li.strolch.utils.Version;
import li.strolch.utils.helper.StringHelper;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.postgresql.Driver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CachedDaoTest extends AbstractModelTest {

	public static final String RUNTIME_PATH = "target/cachedRuntime/";
	public static final String DB_STORE_PATH_DIR = "dbStore";
	public static final String CONFIG_SRC = "src/test/resources/cachedRuntime";

	public static final String DB_URL = "jdbc:postgresql://localhost/testdb";
	public static final String DB_USERNAME = "testuser";
	public static final String DB_PASSWORD = "test";

	private static final Logger logger = LoggerFactory.getLogger(CachedDaoTest.class);

	protected static RuntimeMock runtimeMock;

	@Override
	protected RuntimeMock getRuntimeMock() {
		return runtimeMock;
	}

	@BeforeClass
	public static void beforeClass() throws Exception {

		dropSchema(CachedDaoTest.class.getSimpleName(), SCRIPT_PREFIX_ARCHIVE, DB_URL, DB_USERNAME, DB_PASSWORD);
		dropSchema(CachedDaoTest.class.getSimpleName(), SCRIPT_PREFIX_STROLCH, DB_URL, DB_USERNAME, DB_PASSWORD);

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		File dbStorePath = new File(rootPath, DB_STORE_PATH_DIR);
		if (!dbStorePath.mkdir())
			throw new IllegalStateException("Failed to created db store path " + dbStorePath);
		runtimeMock.startContainer();

		PostgreSqlPersistenceHandler persistenceHandler = (PostgreSqlPersistenceHandler) runtimeMock.getContainer()
				.getComponent(PersistenceHandler.class);
		assertEquals(DataType.xml, persistenceHandler.getDataType());
	}

	public static void dropSchema(String ctx, String scriptPrefix, String dbUrl, String dbUsername, String dbPassword)
			throws Exception {

		if (System.getProperties().containsKey(PROP_DB_HOST_OVERRIDE))
			dbUrl = overridePostgresqlHost(ctx, dbUrl);

		if (!Driver.isRegistered())
			Driver.register();

		Version dbVersion = DbSchemaVersionCheck.getExpectedDbVersion(scriptPrefix, PostgreSqlPersistenceHandler.class);
		logger.info("Dropping schema for expected version {}", dbVersion);
		String sql = DbSchemaVersionCheck.getSql(scriptPrefix, PostgreSqlPersistenceHandler.class, dbVersion, "drop");
		logger.info(StringHelper.NEW_LINE + "{}", sql);
		try (Connection connection = DriverManager.getConnection(dbUrl, dbUsername, dbPassword)) {
			connection.prepareStatement(sql).execute();
		}
	}

	@AfterClass
	public static void afterClass() {
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();
	}
}
