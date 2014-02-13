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
package li.strolch.persistence.impl.dao.test;

import static li.strolch.model.ModelGenerator.createOrder;
import static li.strolch.model.ModelGenerator.createResource;
import static org.junit.Assert.assertEquals;

import java.io.File;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.StrolchElement;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.observer.Observer;
import li.strolch.runtime.observer.ObserverHandler;
import li.strolch.testbase.runtime.RuntimeMock;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import ch.eitchnet.xmlpers.api.ModificationResult;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ObserverUpdateTest {

	public static final String RUNTIME_PATH = "target/observerUpdateStrolchRuntime/"; //$NON-NLS-1$
	public static final String DB_STORE_PATH_DIR = "dbStore"; //$NON-NLS-1$
	public static final String CONFIG_SRC = "src/test/resources/cachedruntime"; //$NON-NLS-1$

	protected static RuntimeMock runtimeMock;

	protected RuntimeMock getRuntimeMock() {
		return runtimeMock;
	}

	@BeforeClass
	public static void beforeClass() {

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

	public final class ElementAddedObserver implements Observer {

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
		public void update(String key, List<StrolchElement> elements) {
			getModificationResult(key).getUpdated().addAll(elements);
		}

		@Override
		public void remove(String key, List<StrolchElement> elements) {
			getModificationResult(key).getDeleted().addAll(elements);
		}

		@Override
		public void add(String key, List<StrolchElement> elements) {
			getModificationResult(key).getCreated().addAll(elements);
		}
	}

	@Test
	public void shouldReceiveUpdates() {

		// register an observer for orders and resources
		ElementAddedObserver observer = new ElementAddedObserver();
		runtimeMock.getContainer().getComponent(ObserverHandler.class).registerObserver("Order", observer); //$NON-NLS-1$
		runtimeMock.getContainer().getComponent(ObserverHandler.class).registerObserver("Resource", observer); //$NON-NLS-1$

		// create order
		Order newOrder = createOrder("MyTestOrder", "Test Name", "TestType", new Date(), State.CREATED); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM).openTx()) {
			tx.getOrderMap().add(tx, newOrder);
		}

		// create resource
		Resource newResource = createResource("MyTestResource", "Test Name", "TestType"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM).openTx()) {
			tx.getResourceMap().add(tx, newResource);
		}

		assertEquals(2, observer.results.size());
		assertEquals(1, observer.results.get("Order").getCreated().size()); //$NON-NLS-1$
		assertEquals(1, observer.results.get("Resource").getCreated().size()); //$NON-NLS-1$

	}
}
