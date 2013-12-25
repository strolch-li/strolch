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
import java.text.MessageFormat;

import li.strolch.model.Resource;
import li.strolch.runtime.agent.ComponentContainer;
import li.strolch.runtime.agent.StrolchAgent;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import li.strolch.runtime.test.component.model.PersistenceHandlerTest;
import li.strolch.runtime.test.component.model.ServiceHandlerTest;
import li.strolch.runtime.test.component.model.ServiceResultTest;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.FileHelper;

@SuppressWarnings("nls")
public class ComponentContainerTest {

	public static final String PATH_REALM_CONTAINER = "src/test/resources/realmtest";
	public static final String PATH_TRANSIENT_CONTAINER = "src/test/resources/transienttest";
	public static final String PATH_EMPTY_CONTAINER = "src/test/resources/emptytest";

	public static final String PATH_REALM_RUNTIME = "target/realmtest/"; //$NON-NLS-1$
	public static final String PATH_TRANSIENT_RUNTIME = "target/transienttest/"; //$NON-NLS-1$
	public static final String PATH_EMPTY_RUNTIME = "target/emptytest/"; //$NON-NLS-1$

	private static final Logger logger = LoggerFactory.getLogger(ComponentContainerTest.class);
	private static final String TARGET = "target"; //$NON-NLS-1$

	@Test
	public void shouldStartEmptyContainer() {

		try {
			StrolchAgent agent = startContainer(PATH_EMPTY_RUNTIME, PATH_EMPTY_CONTAINER);
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
			StrolchAgent agent = startContainer(PATH_TRANSIENT_RUNTIME, PATH_TRANSIENT_CONTAINER);
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
			StrolchAgent agent = startContainer(PATH_REALM_RUNTIME, PATH_REALM_CONTAINER);
			testContainer(agent);
			destroyContainer(agent);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
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

	public static StrolchAgent startContainer(String rootPath, String configSrc) {
		File rootPathF = new File(rootPath);
		File configSrcF = new File(configSrc);
		mockRuntime(rootPathF, configSrcF);
		return startContainer(rootPathF);
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

	public static void mockRuntime(File rootPathF, File rootSrc) {

		if (!rootPathF.getParentFile().getName().equals(TARGET)) {
			String msg = "Mocking path must be in a maven target: {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, rootPathF.getAbsolutePath());
			throw new RuntimeException(msg);
		}

		File configSrc = new File(rootSrc, RuntimeConfiguration.PATH_CONFIG);
		File dataSrc = new File(rootSrc, RuntimeConfiguration.PATH_DATA);

		if (!configSrc.isDirectory() || !configSrc.canRead()) {
			String msg = "Could not find config source in: {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, configSrc.getAbsolutePath());
			throw new RuntimeException(msg);
		}

		if (rootPathF.exists()) {
			logger.info("Deleting all files in " + rootPathF.getAbsolutePath()); //$NON-NLS-1$
			if (!FileHelper.deleteFile(rootPathF, true)) {
				String msg = "Failed to delete {0}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, rootPathF.getAbsolutePath());
				throw new RuntimeException(msg);
			}
		}

		if (!rootPathF.mkdirs()) {
			String msg = "Failed to create {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, rootPathF.getAbsolutePath());
			throw new RuntimeException(msg);
		}

		File configPathF = new File(rootPathF, RuntimeConfiguration.PATH_CONFIG);
		configPathF.mkdir();

		if (!FileHelper.copy(configSrc.listFiles(), configPathF, false)) {
			String msg = "Failed to copy source configs from {0} to {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, configSrc.getAbsolutePath(), configPathF.getAbsolutePath());
			throw new RuntimeException(msg);
		}

		if (dataSrc.exists()) {
			File dataPathF = new File(rootPathF, RuntimeConfiguration.PATH_DATA);
			dataPathF.mkdir();

			if (!FileHelper.copy(dataSrc.listFiles(), dataPathF, false)) {
				String msg = "Failed to copy source data from {0} to {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, configSrc.getAbsolutePath(), configPathF.getAbsolutePath());
				throw new RuntimeException(msg);
			}
		}
	}
}
