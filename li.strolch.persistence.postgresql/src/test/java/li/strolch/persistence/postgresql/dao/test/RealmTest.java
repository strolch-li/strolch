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

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import li.strolch.agent.api.StrolchRealm;
import li.strolch.agent.impl.DataStoreMode;
import li.strolch.model.ModelGenerator;
import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.testbase.runtime.AbstractModelTest;
import li.strolch.testbase.runtime.RuntimeMock;

public class RealmTest extends AbstractModelTest {

	private static final String TESTUSER2 = "testuser2"; //$NON-NLS-1$
	private static final String TESTUSER1 = "testuser1"; //$NON-NLS-1$
	private static final String SECOND = "second"; //$NON-NLS-1$
	private static final String TEST = "test"; //$NON-NLS-1$
	private static final String FIRST = "first"; //$NON-NLS-1$

	public static final String RUNTIME_PATH = "target/realmtest/"; //$NON-NLS-1$
	public static final String DB_STORE_PATH_DIR = "dbStore"; //$NON-NLS-1$
	public static final String CONFIG_SRC = "src/test/resources/realmtest"; //$NON-NLS-1$

	protected static RuntimeMock runtimeMock;

	@Override
	protected RuntimeMock getRuntimeMock() {
		return runtimeMock;
	}

	@BeforeClass
	public static void beforeClass() throws Exception {

		dropSchema("jdbc:postgresql://localhost/testdb1", TESTUSER1, TEST); //$NON-NLS-1$
		dropSchema("jdbc:postgresql://localhost/testdb2", TESTUSER2, TEST); //$NON-NLS-1$

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		new File(rootPath, DB_STORE_PATH_DIR).mkdir();
		runtimeMock.startContainer();
	}

	@Before
	public void before() {
		this.realmName = SECOND;
	}

	@Test
	public void testDifferentRealms() {

		String expectedId1 = "@realmTestId1"; //$NON-NLS-1$
		String expectedId2 = "@realmTestId2"; //$NON-NLS-1$
		String type = "Bla"; //$NON-NLS-1$

		PrivilegeHandler privilegeHandler = runtimeMock.getAgent().getContainer().getPrivilegeHandler();
		Certificate certificate = privilegeHandler.authenticate(TEST, TEST.getBytes());

		{
			StrolchRealm firstRealm = runtimeMock.getRealm(FIRST);
			assertEquals(DataStoreMode.TRANSACTIONAL, firstRealm.getMode());
			Resource expectedRes1 = ModelGenerator.createResource(expectedId1, "Bla bla", type); //$NON-NLS-1$
			try (StrolchTransaction tx = firstRealm.openTx(certificate, TEST)) {
				tx.getResourceMap().add(tx, expectedRes1);
				tx.commitOnClose();
			}

			try (StrolchTransaction tx = firstRealm.openTx(certificate, TEST)) {
				Resource res = tx.getResourceMap().getBy(tx, type, expectedId1);
				assertEquals("Should find object previously added in same realm!", expectedRes1, res); //$NON-NLS-1$
			}
		}

		{
			StrolchRealm secondRealm = runtimeMock.getRealm(SECOND);
			assertEquals(DataStoreMode.TRANSACTIONAL, secondRealm.getMode());
			Resource expectedRes2 = ModelGenerator.createResource(expectedId2, "Bla bla", type); //$NON-NLS-1$
			try (StrolchTransaction tx = secondRealm.openTx(certificate, TEST)) {
				tx.getResourceMap().add(tx, expectedRes2);
				tx.commitOnClose();
			}

			try (StrolchTransaction tx = secondRealm.openTx(certificate, TEST)) {
				Resource res = tx.getResourceMap().getBy(tx, type, expectedId2);
				assertEquals("Should find object previously added in same realm!", expectedRes2, res); //$NON-NLS-1$
			}
		}

		{
			StrolchRealm secondRealm = runtimeMock.getRealm(SECOND);
			try (StrolchTransaction tx = secondRealm.openTx(certificate, TEST)) {
				Resource res = tx.getResourceMap().getBy(tx, type, expectedId1);
				assertNull("Should not find object added in differenct realm!", res); //$NON-NLS-1$
			}
		}
	}

	@AfterClass
	public static void afterClass() {
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();
	}
}
