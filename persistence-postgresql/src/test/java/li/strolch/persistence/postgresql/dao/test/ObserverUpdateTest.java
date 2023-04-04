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

import static li.strolch.model.ModelGenerator.createOrder;
import static li.strolch.model.ModelGenerator.createResource;
import static li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler.SCRIPT_PREFIX_ARCHIVE;
import static li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler.SCRIPT_PREFIX_STROLCH;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.*;
import static org.junit.Assert.assertEquals;

import java.io.File;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import li.strolch.agent.api.Observer;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.*;
import li.strolch.persistence.api.ModificationResult;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.postgresql.DataType;
import li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.testbase.runtime.RuntimeMock;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ObserverUpdateTest {

	public static final String RUNTIME_PATH = "target/observerUpdateStrolchRuntime/";
	public static final String DB_STORE_PATH_DIR = "dbStore";
	public static final String CONFIG_SRC = "src/test/resources/cachedRuntime";

	protected static RuntimeMock runtimeMock;

	@BeforeClass
	public static void beforeClass() throws Exception {

		dropSchema(ObserverUpdateTest.class.getSimpleName(), SCRIPT_PREFIX_ARCHIVE, DB_URL, DB_USERNAME, DB_PASSWORD);
		dropSchema(ObserverUpdateTest.class.getSimpleName(), SCRIPT_PREFIX_STROLCH, DB_URL, DB_USERNAME, DB_PASSWORD);

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

	@AfterClass
	public static void afterClass() {
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();
	}

	public static final class ElementAddedObserver implements Observer {

		Map<String, ModificationResult> results = new HashMap<>();

		private ModificationResult getModificationResult(String key) {
			ModificationResult result = this.results.get(key);
			if (result == null) {
				result = new ModificationResult(key);
				this.results.put(key, result);
			}
			return result;
		}

		@Override
		public void update(String key, List<StrolchRootElement> elements) {
			getModificationResult(key).getUpdated().addAll(elements);
		}

		@Override
		public void remove(String key, List<StrolchRootElement> elements) {
			getModificationResult(key).getDeleted().addAll(elements);
		}

		@Override
		public void add(String key, List<StrolchRootElement> elements) {
			getModificationResult(key).getCreated().addAll(elements);
		}
	}

	@Test
	public void shouldReceiveUpdates() throws InterruptedException {

		// register an observer for orders and resources
		ElementAddedObserver observer = new ElementAddedObserver();
		StrolchRealm realm = runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM);
		realm.getObserverHandler().registerObserver(Tags.ORDER, observer);
		realm.getObserverHandler().registerObserver(Tags.RESOURCE, observer);

		PrivilegeHandler privilegeHandler = runtimeMock.getAgent().getContainer().getPrivilegeHandler();
		Certificate certificate = privilegeHandler
				.authenticate("test", "test".toCharArray()); //$NON-NLS-2$

		// create order
		Order newOrder = createOrder("MyTestOrder", "Test Name", "TestType", new Date(),
				State.CREATED);//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = realm.openTx(certificate, "test", false)) {
			tx.add(newOrder);
			tx.commitOnClose();
		}

		// create resource
		Resource newResource = createResource("MyTestResource", "Test Name",
				"TestType");//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = realm.openTx(certificate, "test", false)) {
			tx.add(newResource);
			tx.commitOnClose();
		}

		// observer updates are async...
		Thread.sleep(200L);

		assertEquals(2, observer.results.size());
		assertEquals(1, observer.results.get(Tags.ORDER).getCreated().size());
		assertEquals(1, observer.results.get(Tags.RESOURCE).getCreated().size());
	}
}
