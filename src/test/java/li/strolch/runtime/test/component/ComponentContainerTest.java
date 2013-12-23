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
package li.strolch.runtime.test.component;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.File;

import li.strolch.model.Resource;
import li.strolch.runtime.agent.ComponentContainer;
import li.strolch.runtime.agent.StrolchAgent;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@SuppressWarnings("nls")
public class ComponentContainerTest {

	public static final String PATH_REALM_CONTAINER = "src/test/resources/realmtest";
	public static final String PATH_TRANSIENT_CONTAINER = "src/test/resources/transienttest";
	public static final String PATH_EMPTY_CONTAINER = "src/test/resources/emptytest";

	private static final Logger logger = LoggerFactory.getLogger(ComponentContainerTest.class);

	@Test
	public void shouldStartEmptyContainer() {

		try {
			StrolchAgent agent = startContainer(PATH_EMPTY_CONTAINER);
			testContainer(agent);
			destroyContainer(agent);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
	}

	@Test
	public void shouldStartTransientContainer() {

		try {
			StrolchAgent agent = startContainer(PATH_TRANSIENT_CONTAINER);
			testContainer(agent);
			destroyContainer(agent);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
	}

	@Test
	public void shouldStartRealmTestContainer() {

		try {
			StrolchAgent agent = startContainer(PATH_REALM_CONTAINER);
			testContainer(agent);
			destroyContainer(agent);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
	}

	public static StrolchAgent startContainer(String rootPath) {
		File rootPathF = new File(rootPath);
		return startContainer(rootPathF);
	}

	private static void testContainer(StrolchAgent agent) {

		ComponentContainer container = agent.getContainer();

		ServiceHandlerTest serviceHandler = container.getComponent(ServiceHandlerTest.class);
		ServiceResultTest result = serviceHandler.doService();
		assertEquals(1, result.getResult());

		PersistenceHandlerTest persistenceHandler = container.getComponent(PersistenceHandlerTest.class);
		Resource resource = persistenceHandler.getTestResource("@testRes", "Test Res", "Test");
		assertNotNull(resource);
		assertEquals("@testRes", resource.getId());
	}

	public static StrolchAgent startContainer(File rootPathF) {
		StrolchAgent agent = new StrolchAgent();
		agent.setup(rootPathF);
		agent.initialize();
		agent.start();

		return agent;
	}

	public static void destroyContainer(StrolchAgent agent) {
		agent.stop();
		agent.destroy();
	}
}
