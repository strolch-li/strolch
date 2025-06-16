/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.RuntimeMock;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.configuration.model.ResourceGeneratorHandlerTest;
import li.strolch.runtime.configuration.model.ServiceHandlerTest;
import li.strolch.runtime.configuration.model.ServiceResultTest;
import li.strolch.runtime.privilege.PrivilegeHandler;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static li.strolch.model.ModelGenerator.*;
import static li.strolch.runtime.StrolchConstants.DEFAULT_REALM;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

@SuppressWarnings("nls")
public class ComponentContainerTest {

	public static final String PATH_CONTAINER_TEST = "src/test/resources/containertest";
	public static final String PATH_TRANSIENT_CONTAINER = "src/test/resources/transienttest";
	public static final String PATH_EMPTY_CONTAINER = "src/test/resources/emptytest";
	public static final String PATH_MINIMAL_CONTAINER = "src/test/resources/minimaltest";

	public static final String PATH_REALM_RUNTIME = "target/realmtest/";
	public static final String PATH_TRANSIENT_RUNTIME = "target/transienttest/";
	public static final String PATH_EMPTY_RUNTIME = "target/emptytest/";

	protected static final Logger logger = LoggerFactory.getLogger(ComponentContainerTest.class);

	@Test
	public void shouldStartEmptyContainer() throws Exception {
		try {
			RuntimeMock.runInStrolch(PATH_EMPTY_RUNTIME, PATH_EMPTY_CONTAINER, ComponentContainerTest::testContainer);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
	}

	@Test
	public void shouldStartTransientContainer() throws Exception {
		try {
			RuntimeMock.runInStrolch(PATH_TRANSIENT_RUNTIME, PATH_TRANSIENT_CONTAINER,
					ComponentContainerTest::testContainer);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
	}

	@Test
	public void shouldStartRealmTestContainer() throws Exception {
		try {
			RuntimeMock.runInStrolch(PATH_REALM_RUNTIME, PATH_CONTAINER_TEST, ComponentContainerTest::testContainer);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
	}

	@Test
	public void shouldTestRealms() throws Exception {
		try {
			RuntimeMock.runInStrolch(PATH_REALM_RUNTIME, PATH_CONTAINER_TEST, (mock, agent) -> {
				testContainer(mock, agent);
				testRealms(agent);
			});
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
	}

	@Test
	public void shouldTestMinimal() throws Exception {
		try {
			RuntimeMock.runInStrolch(PATH_REALM_RUNTIME, PATH_MINIMAL_CONTAINER, (_, agent) -> {
				ComponentContainer container = agent.getContainer();
				ServiceHandlerTest serviceHandler = container.getComponent(ServiceHandlerTest.class);
				ServiceResultTest result = serviceHandler.doService();
				assertEquals(1, result.result());
			});
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
	}

	public static void testContainer(RuntimeMock mock, StrolchAgent agent) {

		ComponentContainer container = agent.getContainer();

		ServiceHandlerTest serviceHandler = container.getComponent(ServiceHandlerTest.class);
		ServiceResultTest result = serviceHandler.doService();
		assertEquals(1, result.result());

		ResourceGeneratorHandlerTest resourceGeneratorHandler = container.getComponent(
				ResourceGeneratorHandlerTest.class);
		Resource resource = resourceGeneratorHandler.getTestResource("@testRes", "Test Res", "Test");
		assertNotNull(resource);
		assertEquals("@testRes", resource.getId());
	}

	static Certificate login(StrolchAgent agent) {
		PrivilegeHandler privilegeHandler = agent.getContainer().getPrivilegeHandler();
		return privilegeHandler.authenticate("test", "test".toCharArray());
	}

	public static void testRealms(StrolchAgent agent) {
		ComponentContainer container = agent.getContainer();
		Certificate certificate = login(agent);
		testDefaultRealm(container, certificate);
	}

	static void testDefaultRealm(ComponentContainer container, Certificate certificate) {
		try (StrolchTransaction tx = container.getRealm(DEFAULT_REALM).openTx(certificate, "test", false)) {
			tx.add(createResource("@testRes1", "Test Res", "Test"));
			Resource queriedRes = tx.getResourceBy("Test", "@testRes1");
			assertNotNull(queriedRes);
			assertEquals("@testRes1", queriedRes.getId());

			tx.add(createOrder("@testOrder1", "Test Order", "Test"));
			Order queriedOrder = tx.getOrderBy("Test", "@testOrder1");
			assertNotNull(queriedOrder);
			assertEquals("@testOrder1", queriedOrder.getId());

			tx.add(createActivity("@testActivity0", "Test Activity", "Test", TimeOrdering.SERIES));
			Activity queriedActivity = tx.getActivityBy("Test", "@testActivity0");
			assertNotNull(queriedActivity);
			assertEquals("@testActivity0", queriedActivity.getId());

			tx.commitOnClose();
		}
	}
}
