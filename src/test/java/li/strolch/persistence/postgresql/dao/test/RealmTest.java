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

import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.dropSchema;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.io.File;
import java.sql.SQLException;

import li.strolch.agent.api.StrolchRealm;
import li.strolch.agent.impl.DataStoreMode;
import li.strolch.model.ModelGenerator;
import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.testbase.runtime.AbstractModelTest;
import li.strolch.testbase.runtime.RuntimeMock;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class RealmTest extends AbstractModelTest {

	public static final String RUNTIME_PATH = "target/realmtest/"; //$NON-NLS-1$
	public static final String DB_STORE_PATH_DIR = "dbStore"; //$NON-NLS-1$
	public static final String CONFIG_SRC = "src/test/resources/realmtest"; //$NON-NLS-1$

	protected static RuntimeMock runtimeMock;

	@Override
	protected RuntimeMock getRuntimeMock() {
		return runtimeMock;
	}

	@BeforeClass
	public static void beforeClass() throws SQLException {

		dropSchema("jdbc:postgresql://localhost/testdb1", "testuser1", "test");
		dropSchema("jdbc:postgresql://localhost/testdb2", "testuser2", "test");

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		new File(rootPath, DB_STORE_PATH_DIR).mkdir();
		runtimeMock.startContainer();
	}

	@Before
	public void before() {
		this.realmName = "second";
	}

	@Test
	public void testDifferentRealms() {

		String expectedId1 = "@realmTestId1";
		String expectedId2 = "@realmTestId2";
		String type = "Bla";

		{
			StrolchRealm firstRealm = runtimeMock.getRealm("first");
			assertEquals(DataStoreMode.TRANSACTIONAL, firstRealm.getMode());
			Resource expectedRes1 = ModelGenerator.createResource(expectedId1, "Bla bla", type);
			try (StrolchTransaction tx = firstRealm.openTx()) {
				tx.getResourceMap().add(tx, expectedRes1);
			}

			try (StrolchTransaction tx = firstRealm.openTx()) {
				Resource res = tx.getResourceMap().getBy(tx, type, expectedId1);
				assertEquals("Should find object previously added in same realm!", expectedRes1, res);
			}
		}

		{
			StrolchRealm secondRealm = runtimeMock.getRealm("second");
			assertEquals(DataStoreMode.TRANSACTIONAL, secondRealm.getMode());
			Resource expectedRes2 = ModelGenerator.createResource(expectedId2, "Bla bla", type);
			try (StrolchTransaction tx = secondRealm.openTx()) {
				tx.getResourceMap().add(tx, expectedRes2);
			}

			try (StrolchTransaction tx = secondRealm.openTx()) {
				Resource res = tx.getResourceMap().getBy(tx, type, expectedId2);
				assertEquals("Should find object previously added in same realm!", expectedRes2, res);
			}
		}

		{
			StrolchRealm secondRealm = runtimeMock.getRealm("second");
			try (StrolchTransaction tx = secondRealm.openTx()) {
				Resource res = tx.getResourceMap().getBy(tx, type, expectedId1);
				assertNull("Should not find object added in differenct realm!", res);
			}
		}
	}

	@AfterClass
	public static void afterClass() {
		runtimeMock.destroyRuntime();
	}
}
