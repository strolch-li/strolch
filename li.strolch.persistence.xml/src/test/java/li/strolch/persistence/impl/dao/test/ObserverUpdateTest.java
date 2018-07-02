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

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import li.strolch.agent.api.Observer;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.testbase.runtime.RuntimeMock;
import li.strolch.xmlpers.api.ModificationResult;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ObserverUpdateTest {

	private static final String TEST = "test"; //$NON-NLS-1$
	public static final String RUNTIME_PATH = "target/observerUpdateStrolchRuntime/"; //$NON-NLS-1$
	public static final String CONFIG_SRC = "src/test/resources/cachedRuntime"; //$NON-NLS-1$

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
	public void shouldReceiveUpdates() {

		// register an observer for orders and resources
		ElementAddedObserver observer = new ElementAddedObserver();
		StrolchRealm realm = runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM);
		realm.getObserverHandler().registerObserver(Tags.ORDER, observer);
		realm.getObserverHandler().registerObserver(Tags.RESOURCE, observer);

		PrivilegeHandler privilegeHandler = runtimeMock.getAgent().getContainer().getPrivilegeHandler();
		Certificate certificate = privilegeHandler.authenticate(TEST, TEST.toCharArray());

		// create order
		Order newOrder = createOrder("MyTestOrder", "Test Name", "TestType", new Date(), State.CREATED);
		try (StrolchTransaction tx = realm.openTx(certificate, TEST)) {
			tx.add(newOrder);
			tx.commitOnClose();
		}

		// create resource
		Resource newResource = createResource("MyTestResource", "Test Name", "TestType");
		try (StrolchTransaction tx = realm.openTx(certificate, TEST)) {
			tx.add(newResource);
			tx.commitOnClose();
		}

		assertEquals(2, observer.results.size());
		assertEquals(1, observer.results.get(Tags.ORDER).getCreated().size());
		assertEquals(1, observer.results.get(Tags.RESOURCE).getCreated().size());
	}
}
