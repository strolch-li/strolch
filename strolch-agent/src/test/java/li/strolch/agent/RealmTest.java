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
import li.strolch.agent.impl.DataStoreMode;
import li.strolch.persistence.api.StrolchTransaction;
import org.junit.Test;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import static li.strolch.agent.ComponentContainerTest.*;
import static org.junit.Assert.assertEquals;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RealmTest {

	@Test
	public void shouldStartRealmTestContainer() throws Exception {

		try {
			RuntimeMock.runInStrolch(PATH_REALM_RUNTIME, PATH_REALM_CONTAINER, (mock, agent) -> {
				testContainer(mock, agent);

				ComponentContainer container = agent.getContainer();
				Set<String> realmNames = container.getRealmNames();
				assertEquals(5, realmNames.size());

				Set<String> expectedRealmNames = new HashSet<>(
						Arrays.asList("defaultRealm", "myRealm", "otherRealm", "emptyRealm", "eclipseStorage"));
				assertEquals(expectedRealmNames, realmNames);

				assertEquals(DataStoreMode.TRANSIENT, container.getRealm("defaultRealm").getMode());
				assertEquals(DataStoreMode.TRANSIENT, container.getRealm("myRealm").getMode());
				assertEquals(DataStoreMode.TRANSIENT, container.getRealm("otherRealm").getMode());
				assertEquals(DataStoreMode.EMPTY, container.getRealm("emptyRealm").getMode());
				assertEquals(DataStoreMode.ECLIPSE_STORAGE, container.getRealm("eclipseStorage").getMode());

				for (String realm : realmNames) {
					agent.runAsAgent(ctx -> {
						try (StrolchTransaction tx = container
								.getRealm(realm)
								.openTx(ctx.getCertificate(), "test", true)) {

							switch (tx.getRealmName()) {
								case "defaultRealm", "eclipseStorage" -> {
									assertEquals("Expected 2 resources in realm " + tx.getRealmName(), 2,
											tx.getResourceCount());
									assertEquals("Expected 2 orders in realm " + tx.getRealmName(), 2,
											tx.getOrderCount());
									assertEquals("Expected 2 activities in realm " + tx.getRealmName(), 2,
											tx.getActivityCount());
								}
								case "otherRealm", "myRealm" -> {
									assertEquals("Expected 1 resources in realm " + tx.getRealmName(), 1,
											tx.getResourceCount());
									assertEquals("Expected 1 orders in realm " + tx.getRealmName(), 1,
											tx.getOrderCount());
									assertEquals("Expected 1 activities in realm " + tx.getRealmName(), 1,
											tx.getActivityCount());
								}
								case "emptyRealm" -> {
									assertEquals("Expected 0 resources in realm " + tx.getRealmName(), 0,
											tx.getResourceCount());
									assertEquals("Expected 0 orders in realm " + tx.getRealmName(), 0,
											tx.getOrderCount());
									assertEquals("Expected 0 activities in realm " + tx.getRealmName(), 0,
											tx.getActivityCount());
								}
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
}
