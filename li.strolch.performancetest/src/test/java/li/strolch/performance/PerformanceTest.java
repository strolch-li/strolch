/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.performance;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.text.MessageFormat;

import li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler;
import li.strolch.service.api.ServiceHandler;
import li.strolch.testbase.runtime.RuntimeMock;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.db.DbSchemaVersionCheck;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.Version;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PerformanceTest {

	public static final String RUNTIME_PATH = "target/transactionalStrolchRuntime/"; //$NON-NLS-1$
	public static final String DB_STORE_PATH_DIR = "dbStore"; //$NON-NLS-1$
	public static final String CONFIG_SRC = "src/runtime"; //$NON-NLS-1$

	public static final String DB_URL = "jdbc:postgresql://localhost/testdb"; //$NON-NLS-1$
	public static final String DB_USERNAME = "testuser"; //$NON-NLS-1$
	public static final String DB_PASSWORD = "test"; //$NON-NLS-1$

	private static final Logger logger = LoggerFactory.getLogger(PerformanceTest.class);

	protected static RuntimeMock runtimeMock;

	public static void dropSchema(String dbUrl, String dbUsername, String dbPassword) throws Exception {
		Version dbVersion = DbSchemaVersionCheck.getExpectedDbVersion(PostgreSqlPersistenceHandler.SCRIPT_PREFIX,
				PostgreSqlPersistenceHandler.class);
		logger.info(MessageFormat.format("Dropping schema for expected version {0}", dbVersion));
		String sql = DbSchemaVersionCheck.getSql(PostgreSqlPersistenceHandler.SCRIPT_PREFIX,
				PostgreSqlPersistenceHandler.class, dbVersion, "drop"); //$NON-NLS-1$
		logger.info(StringHelper.NEW_LINE + sql);
		try (Connection connection = DriverManager.getConnection(dbUrl, dbUsername, dbPassword)) {
			connection.prepareStatement(sql).execute();
		}
	}

	@BeforeClass
	public static void beforeClass() throws Exception {

		dropSchema(DB_URL, DB_USERNAME, DB_PASSWORD);

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		new File(rootPath, DB_STORE_PATH_DIR).mkdir();
		runtimeMock.startContainer();
	}

	@AfterClass
	public static void afterClass() {
		runtimeMock.destroyRuntime();
	}

	@Test
	public void runPerformanceTest() {

		Certificate certificate = runtimeMock.getPrivilegeHandler().authenticate("test", "test".getBytes());

		ServiceHandler svcHandler = runtimeMock.getServiceHandler();
		svcHandler.doService(certificate, new PerformanceTestService());
	}
}
