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
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.dropSchema;
import static org.junit.Assert.assertEquals;

import java.io.File;

import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.postgresql.DataType;
import li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler;
import li.strolch.testbase.runtime.AbstractModelTest;
import li.strolch.testbase.runtime.RuntimeMock;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CachedVersioningDaoTest extends AbstractModelTest {

	public static final String RUNTIME_PATH = "target/cachedRuntimeVersioning/";
	public static final String DB_STORE_PATH_DIR = "dbStore";
	public static final String CONFIG_SRC = "src/test/resources/cachedRuntimeVersioning";

	public static final String DB_URL = "jdbc:postgresql://localhost/testdb";
	public static final String DB_USERNAME = "testuser";
	public static final String DB_PASSWORD = "test";

	private static final Logger logger = LoggerFactory.getLogger(CachedVersioningDaoTest.class);

	protected static RuntimeMock runtimeMock;

	@Override
	protected RuntimeMock getRuntimeMock() {
		return runtimeMock;
	}

	@BeforeClass
	public static void beforeClass() throws Exception {

		dropSchema(CachedVersioningDaoTest.class.getSimpleName(), SCRIPT_PREFIX_ARCHIVE, DB_URL, DB_USERNAME, DB_PASSWORD);
		dropSchema(CachedVersioningDaoTest.class.getSimpleName(), SCRIPT_PREFIX_STROLCH, DB_URL, DB_USERNAME, DB_PASSWORD);

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		new File(rootPath, DB_STORE_PATH_DIR).mkdir();
		runtimeMock.startContainer();

		PostgreSqlPersistenceHandler persistenceHandler = (PostgreSqlPersistenceHandler) runtimeMock.getContainer()
				.getComponent(PersistenceHandler.class);
		assertEquals(DataType.xml, persistenceHandler.getDataType());
	}

	@AfterClass
	public static void afterClass() {
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();
	}
}
