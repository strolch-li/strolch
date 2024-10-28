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

import li.strolch.RuntimeMock;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.impl.DataStoreMode;
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
			RuntimeMock.runInStrolch(PATH_REALM_RUNTIME, PATH_REALM_CONTAINER, agent -> {
				testContainer(agent);

				ComponentContainer container = agent.getContainer();
				Set<String> realmNames = container.getRealmNames();
				assertEquals(4, realmNames.size());

				Set<String> expectedRealmNames = new HashSet<>(
						Arrays.asList("defaultRealm", "myRealm", "otherRealm", "emptyRealm"));
				assertEquals(expectedRealmNames, realmNames);

				assertEquals(DataStoreMode.TRANSIENT, container.getRealm("defaultRealm").getMode());
				assertEquals(DataStoreMode.TRANSIENT, container.getRealm("myRealm").getMode());
				assertEquals(DataStoreMode.TRANSIENT, container.getRealm("otherRealm").getMode());
				assertEquals(DataStoreMode.EMPTY, container.getRealm("emptyRealm").getMode());
			});
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
	}
}
