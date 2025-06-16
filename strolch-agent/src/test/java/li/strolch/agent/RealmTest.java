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
import li.strolch.agent.impl.DataStoreMode;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import org.junit.Test;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import static li.strolch.agent.ComponentContainerTest.*;
import static org.junit.Assert.*;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RealmTest {

	public static final String PATH_REALM_CONTAINER = "src/test/resources/realmtest";

	@Test
	public void shouldStartRealmTestContainer() throws Exception {

		try {
			RuntimeMock.runInStrolch(PATH_REALM_RUNTIME, PATH_REALM_CONTAINER, (mock, agent) -> {
				testContainer(mock, agent);

				ComponentContainer container = agent.getContainer();
				Certificate certificate = login(agent);
				testDefaultRealm(container, certificate);

				Set<String> realmNames = container.getRealmNames();
				assertEquals(5, realmNames.size());

				Set<String> expectedRealmNames = new HashSet<>(
						Arrays.asList("defaultRealm", "myRealm", "otherRealm", "emptyRealm", "eclipseStore"));
				assertEquals(expectedRealmNames, realmNames);

				assertEquals(DataStoreMode.TRANSIENT, container.getRealm("defaultRealm").getMode());
				assertEquals(DataStoreMode.TRANSIENT, container.getRealm("myRealm").getMode());
				assertEquals(DataStoreMode.TRANSIENT, container.getRealm("otherRealm").getMode());
				assertEquals(DataStoreMode.EMPTY, container.getRealm("emptyRealm").getMode());
				assertEquals(DataStoreMode.ECLIPSE_STORE, container.getRealm("eclipseStore").getMode());

				for (String realm : realmNames) {
					agent.runAsAgent(ctx -> {
						try (StrolchTransaction tx = container
								.getRealm(realm)
								.openTx(ctx.getCertificate(), "test", true)) {
							switch (tx.getRealmName()) {
								case "defaultRealm" -> assertElements(3, tx);
								case "eclipseStore" -> assertElements(2, tx);
								case "otherRealm", "myRealm" -> assertElements(1, tx);
								case "emptyRealm" -> assertElements(0, tx);
							}
						}
					});
				}
			});
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
	}

	private static void assertElements(int expectedCount, StrolchTransaction tx) {
		assertEquals("Expected " + expectedCount + " resources in realm " + tx.getRealmName(), expectedCount,
				tx.getResourceCount());
		assertEquals("Expected " + expectedCount + " orders in realm " + tx.getRealmName(), expectedCount,
				tx.getOrderCount());
		assertEquals("Expected " + expectedCount + " activities in realm " + tx.getRealmName(), expectedCount,
				tx.getActivityCount());
	}

	public static void testRealms(StrolchAgent agent) {

		ComponentContainer container = agent.getContainer();

		Certificate certificate = login(agent);

		testDefaultRealm(container, certificate);
		testMyRealm(container, certificate);
		testOtherRealm(container, certificate);
	}

	private static void testMyRealm(ComponentContainer container, Certificate certificate) {
		try (StrolchTransaction tx = container.getRealm("myRealm").openTx(certificate, "test", false)) {
			Resource myRealmRes = tx.getResourceBy("TestType", "MyRealmRes");
			assertNotNull(myRealmRes);
			assertEquals("MyRealmRes", myRealmRes.getId());
			Resource otherRealmRes = tx.getResourceBy("TestType", "OtherRealmRes");
			assertNull(otherRealmRes);

			Order myRealmOrder = tx.getOrderBy("TestType", "MyRealmOrder");
			assertNotNull(myRealmOrder);
			assertEquals("MyRealmOrder", myRealmOrder.getId());
			Order otherRealmOrder = tx.getOrderBy("TestType", "OtherRealmOrder");
			assertNull(otherRealmOrder);

			Activity myRealmAct = tx.getActivityBy("TestType", "MyRealmAct");
			assertNotNull(myRealmAct);
			assertEquals("MyRealmAct", myRealmAct.getId());
			Activity otherRealmAct = tx.getActivityBy("TestType", "OtherRealmAct");
			assertNull(otherRealmAct);

			tx.commitOnClose();
		}
	}

	private static void testOtherRealm(ComponentContainer container, Certificate certificate) {
		try (StrolchTransaction tx = container.getRealm("otherRealm").openTx(certificate, "test", false)) {
			Resource otherRealmRes = tx.getResourceBy("TestType", "OtherRealmRes");
			assertNotNull(otherRealmRes);
			assertEquals("OtherRealmRes", otherRealmRes.getId());
			Resource myRealmRes = tx.getResourceBy("TestType", "MyRealmRes");
			assertNull(myRealmRes);

			Order otherRealmOrder = tx.getOrderBy("TestType", "OtherRealmOrder");
			assertNotNull(otherRealmOrder);
			assertEquals("OtherRealmOrder", otherRealmOrder.getId());
			Order myRealmOrder = tx.getOrderBy("TestType", "MyRealmOrder");
			assertNull(myRealmOrder);
			tx.commitOnClose();

			Activity otherRealmAct = tx.getActivityBy("TestType", "OtherRealmAct");
			assertNotNull(otherRealmAct);
			assertEquals("OtherRealmAct", otherRealmAct.getId());
			Activity myRealmAct = tx.getActivityBy("TestType", "MyRealmAct");
			assertNull(myRealmAct);

			tx.commitOnClose();
		}
	}
}
