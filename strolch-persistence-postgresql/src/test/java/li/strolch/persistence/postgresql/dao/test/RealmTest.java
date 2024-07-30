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
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.DB_PASSWORD;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.dropSchema;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.io.File;

import li.strolch.agent.api.StrolchRealm;
import li.strolch.agent.impl.DataStoreMode;
import li.strolch.model.ModelGenerator;
import li.strolch.model.Resource;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.postgresql.DataType;
import li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.testbase.runtime.AbstractModelTest;
import li.strolch.testbase.runtime.RuntimeMock;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class RealmTest extends AbstractModelTest {

	public static final String DB_URL1 = "jdbc:postgresql://localhost/testdb1";
	public static final String DB_URL2 = "jdbc:postgresql://localhost/testdb2";

	private static final String TEST_USER2 = "testuser2";
	private static final String TEST_USER1 = "testuser1";
	private static final String TEST = "test";
	private static final String FIRST = "first";
	private static final String SECOND = "second";

	public static final String RUNTIME_PATH = "target/realmtest/";
	public static final String DB_STORE_PATH_DIR = "dbStore";
	public static final String CONFIG_SRC = "src/test/resources/realmtest";

	protected static RuntimeMock runtimeMock;

	@Override
	protected RuntimeMock getRuntimeMock() {
		return runtimeMock;
	}

	@BeforeClass
	public static void beforeClass() throws Exception {

		dropSchema(RealmTest.class.getSimpleName(), SCRIPT_PREFIX_ARCHIVE, DB_URL1, TEST_USER1, DB_PASSWORD);
		dropSchema(RealmTest.class.getSimpleName(), SCRIPT_PREFIX_STROLCH, DB_URL1, TEST_USER1, DB_PASSWORD);
		dropSchema(RealmTest.class.getSimpleName(), SCRIPT_PREFIX_ARCHIVE, DB_URL2, TEST_USER2, DB_PASSWORD);
		dropSchema(RealmTest.class.getSimpleName(), SCRIPT_PREFIX_STROLCH, DB_URL2, TEST_USER2, DB_PASSWORD);

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		if (!new File(rootPath, DB_STORE_PATH_DIR).mkdir())
			throw new IllegalStateException("Failed to create dir " + rootPath + "/" + DB_STORE_PATH_DIR);
		runtimeMock.startContainer();

		PostgreSqlPersistenceHandler persistenceHandler = (PostgreSqlPersistenceHandler) runtimeMock.getContainer()
				.getComponent(PersistenceHandler.class);
		assertEquals(DataType.xml, persistenceHandler.getDataType());
	}

	@Before
	public void before() {
		this.realmName = SECOND;
	}

	@Test
	public void testDifferentRealms() {

		String expectedId1 = "@realmTestId1";
		String expectedId2 = "@realmTestId2";
		String type = "Bla";

		PrivilegeHandler privilegeHandler = runtimeMock.getAgent().getContainer().getPrivilegeHandler();
		Certificate certificate = privilegeHandler.authenticate(TEST, TEST.toCharArray());

		{
			StrolchRealm firstRealm = runtimeMock.getRealm(FIRST);
			assertEquals(DataStoreMode.CACHED, firstRealm.getMode());
			Resource expectedRes1 = ModelGenerator.createResource(expectedId1, "Bla bla", type);
			try (StrolchTransaction tx = firstRealm.openTx(certificate, TEST, false)) {
				tx.add(expectedRes1);
				tx.commitOnClose();
			}

			try (StrolchTransaction tx = firstRealm.openTx(certificate, TEST, true)) {
				Resource res = tx.getResourceBy(type, expectedId1);
				assertEquals("Should find object previously added in same realm!", expectedRes1, res);
			}
		}

		{
			StrolchRealm secondRealm = runtimeMock.getRealm(SECOND);
			assertEquals(DataStoreMode.CACHED, secondRealm.getMode());
			Resource expectedRes2 = ModelGenerator.createResource(expectedId2, "Bla bla", type);
			try (StrolchTransaction tx = secondRealm.openTx(certificate, TEST, false)) {
				tx.add(expectedRes2);
				tx.commitOnClose();
			}

			try (StrolchTransaction tx = secondRealm.openTx(certificate, TEST, true)) {
				Resource res = tx.getResourceBy(type, expectedId2);
				assertEquals("Should find object previously added in same realm!", expectedRes2, res);
			}
		}

		{
			StrolchRealm secondRealm = runtimeMock.getRealm(SECOND);
			try (StrolchTransaction tx = secondRealm.openTx(certificate, TEST, true)) {
				Resource res = tx.getResourceBy(type, expectedId1);
				assertNull("Should not find object added in differenct realm!", res);
			}
		}
	}

	@AfterClass
	public static void afterClass() {
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();
	}
}
