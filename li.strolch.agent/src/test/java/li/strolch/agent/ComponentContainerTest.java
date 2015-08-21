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
package li.strolch.agent;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.model.Certificate;
import li.strolch.RuntimeMock;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.model.ModelGenerator;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.configuration.model.ResourceGeneratorHandlerTest;
import li.strolch.runtime.configuration.model.ServiceHandlerTest;
import li.strolch.runtime.configuration.model.ServiceResultTest;
import li.strolch.runtime.privilege.PrivilegeHandler;

@SuppressWarnings("nls")
public class ComponentContainerTest {

	public static final String PATH_REALM_CONTAINER = "src/test/resources/realmtest";
	public static final String PATH_TRANSIENT_CONTAINER = "src/test/resources/transienttest";
	public static final String PATH_TRANSACTIONAL_CONTAINER = "src/test/resources/transactionaltest";
	public static final String PATH_CACHED_CONTAINER = "src/test/resources/cachedtest";
	public static final String PATH_EMPTY_CONTAINER = "src/test/resources/emptytest";
	public static final String PATH_MINIMAL_CONTAINER = "src/test/resources/minimaltest";

	public static final String PATH_REALM_RUNTIME = "target/realmtest/";
	public static final String PATH_TRANSIENT_RUNTIME = "target/transienttest/";
	public static final String PATH_TRANSACTIONAL_RUNTIME = "target/transactionaltest/";
	public static final String PATH_CACHED_RUNTIME = "target/cachedtest/";
	public static final String PATH_EMPTY_RUNTIME = "target/emptytest/";

	protected static final Logger logger = LoggerFactory.getLogger(ComponentContainerTest.class);

	@Test
	public void shouldStartEmptyContainer() {

		try {
			RuntimeMock.runInStrolch(PATH_EMPTY_RUNTIME, PATH_EMPTY_CONTAINER, agent -> testContainer(agent));
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
	}

	@Test
	public void shouldStartTransientContainer() {

		try {
			RuntimeMock.runInStrolch(PATH_TRANSIENT_RUNTIME, PATH_TRANSIENT_CONTAINER, agent -> testContainer(agent));
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
	}

	@Test
	public void shouldStartTransactionalContainer() {

		try {
			RuntimeMock.runInStrolch(PATH_TRANSACTIONAL_RUNTIME, PATH_TRANSACTIONAL_CONTAINER, agent -> {
				testPersistenceContainer(agent);
				testElementMaps(agent);
			});
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
	}

	@Test
	public void shouldStartCachedContainer() {

		try {
			RuntimeMock.runInStrolch(PATH_CACHED_RUNTIME, PATH_CACHED_CONTAINER, agent -> {
				testPersistenceContainer(agent);
				testElementMaps(agent);
			});
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
	}

	@Test
	public void shouldStartRealmTestContainer() {

		try {
			RuntimeMock.runInStrolch(PATH_REALM_RUNTIME, PATH_REALM_CONTAINER, agent -> testContainer(agent));
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
	}

	@Test
	public void shouldTestRealms() {

		try {
			RuntimeMock.runInStrolch(PATH_REALM_RUNTIME, PATH_REALM_CONTAINER, agent -> {
				testContainer(agent);
				testRealms(agent);
			});
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
	}

	@Test
	public void shouldTestMinimal() {

		try {
			RuntimeMock.runInStrolch(PATH_REALM_RUNTIME, PATH_MINIMAL_CONTAINER, agent -> {
				ComponentContainer container = agent.getContainer();
				ServiceHandlerTest serviceHandler = container.getComponent(ServiceHandlerTest.class);
				ServiceResultTest result = serviceHandler.doService();
				assertEquals(1, result.getResult());
			});
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
	}

	public static void testContainer(StrolchAgent agent) {

		ComponentContainer container = agent.getContainer();

		ServiceHandlerTest serviceHandler = container.getComponent(ServiceHandlerTest.class);
		ServiceResultTest result = serviceHandler.doService();
		assertEquals(1, result.getResult());

		ResourceGeneratorHandlerTest resourceGeneratorHandler = container
				.getComponent(ResourceGeneratorHandlerTest.class);
		Resource resource = resourceGeneratorHandler.getTestResource("@testRes", "Test Res", "Test");
		assertNotNull(resource);
		assertEquals("@testRes", resource.getId());
	}

	private static Certificate login(StrolchAgent agent) {
		PrivilegeHandler privilegeHandler = agent.getContainer().getPrivilegeHandler();
		return privilegeHandler.authenticate("test", "test".getBytes());
	}

	public static void testPersistenceContainer(StrolchAgent agent) {

		ComponentContainer container = agent.getContainer();

		ServiceHandlerTest serviceHandler = container.getComponent(ServiceHandlerTest.class);
		ServiceResultTest result = serviceHandler.doService();
		assertEquals(1, result.getResult());

		Certificate certificate = login(agent);
		try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM).openTx(certificate, "test")) {
			ResourceDao resourceDao = tx.getPersistenceHandler().getResourceDao(tx);
			resourceDao.save(ModelGenerator.createResource("@testRes0", "Test Res", "Test"));
			Resource queriedRes = resourceDao.queryBy("Test", "@testRes0");
			assertNotNull(queriedRes);
			assertEquals("@testRes0", queriedRes.getId());
			tx.commitOnClose();
		}

		try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM).openTx(certificate, "test")) {
			OrderDao orderDao = tx.getPersistenceHandler().getOrderDao(tx);
			orderDao.save(ModelGenerator.createOrder("@testOrder0", "Test Order", "Test"));
			Order queriedOrder = orderDao.queryBy("Test", "@testOrder0");
			assertNotNull(queriedOrder);
			assertEquals("@testOrder0", queriedOrder.getId());
			tx.commitOnClose();
		}
	}

	public static void testElementMaps(StrolchAgent agent) {

		ComponentContainer container = agent.getContainer();

		Certificate certificate = login(agent);
		try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM).openTx(certificate, "test")) {
			ResourceMap resourceMap = tx.getResourceMap();
			resourceMap.add(tx, ModelGenerator.createResource("@testRes1", "Test Res", "Test"));
			Resource queriedRes = resourceMap.getBy(tx, "Test", "@testRes1");
			assertNotNull(queriedRes);
			assertEquals("@testRes1", queriedRes.getId());
			tx.commitOnClose();
		}

		try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM).openTx(certificate, "test")) {
			OrderMap orderMap = tx.getOrderMap();
			orderMap.add(tx, ModelGenerator.createOrder("@testOrder1", "Test Order", "Test"));
			Order queriedOrder = orderMap.getBy(tx, "Test", "@testOrder1");
			assertNotNull(queriedOrder);
			assertEquals("@testOrder1", queriedOrder.getId());
			tx.commitOnClose();
		}
	}

	public static void testRealms(StrolchAgent agent) {

		ComponentContainer container = agent.getContainer();

		Certificate certificate = login(agent);
		try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM).openTx(certificate, "test")) {
			ResourceMap resourceMap = tx.getResourceMap();
			resourceMap.add(tx, ModelGenerator.createResource("@testRes1", "Test Res", "Test"));
			Resource queriedRes = resourceMap.getBy(tx, "Test", "@testRes1");
			assertNotNull(queriedRes);
			assertEquals("@testRes1", queriedRes.getId());

			OrderMap orderMap = tx.getOrderMap();
			orderMap.add(tx, ModelGenerator.createOrder("@testOrder1", "Test Order", "Test"));
			Order queriedOrder = orderMap.getBy(tx, "Test", "@testOrder1");
			assertNotNull(queriedOrder);
			assertEquals("@testOrder1", queriedOrder.getId());
			tx.commitOnClose();
		}

		try (StrolchTransaction tx = container.getRealm("myRealm").openTx(certificate, "test")) {
			ResourceMap resourceMap = tx.getResourceMap();
			Resource myRealmRes = resourceMap.getBy(tx, "TestType", "MyRealmRes");
			assertNotNull(myRealmRes);
			assertEquals("MyRealmRes", myRealmRes.getId());
			Resource otherRealmRes = resourceMap.getBy(tx, "TestType", "OtherRealmRes");
			assertNull(otherRealmRes);

			OrderMap orderMap = tx.getOrderMap();
			Order myRealmOrder = orderMap.getBy(tx, "TestType", "MyRealmOrder");
			assertNotNull(myRealmOrder);
			assertEquals("MyRealmOrder", myRealmOrder.getId());
			Order otherRealmOrder = orderMap.getBy(tx, "TestType", "OtherRealmOrder");
			assertNull(otherRealmOrder);
			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm("otherRealm").openTx(certificate, "test")) {
			ResourceMap resourceMap = tx.getResourceMap();
			Resource otherRealmRes = resourceMap.getBy(tx, "TestType", "OtherRealmRes");
			assertNotNull(otherRealmRes);
			assertEquals("OtherRealmRes", otherRealmRes.getId());
			Resource myRealmRes = resourceMap.getBy(tx, "TestType", "MyRealmRes");
			assertNull(myRealmRes);

			OrderMap orderMap = tx.getOrderMap();
			Order otherRealmOrder = orderMap.getBy(tx, "TestType", "OtherRealmOrder");
			assertNotNull(otherRealmOrder);
			assertEquals("OtherRealmOrder", otherRealmOrder.getId());
			Order myRealmOrder = orderMap.getBy(tx, "TestType", "MyRealmOrder");
			assertNull(myRealmOrder);
			tx.commitOnClose();
		}
	}
}
