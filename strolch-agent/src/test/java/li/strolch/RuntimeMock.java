/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.InputStream;
import java.text.MessageFormat;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchBootstrapper;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.agent.api.StrolchVersion;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;
import li.strolch.utils.helper.FileHelper;
import li.strolch.utils.helper.StringHelper;

/**
 * Basically you should use the RuntimeMock class in the testbase project, but to mitigate circular dependencies, in
 * tests of the agent project we use this implementation
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RuntimeMock implements AutoCloseable {

	private static final Logger logger = LoggerFactory.getLogger(RuntimeMock.class);
	private static final String TARGET = "target";

	private ComponentContainer container;
	private StrolchAgent agent;
	private final File targetPathF;
	private final File srcPathF;

	public RuntimeMock(String targetPath, String srcPath) {
		this.targetPathF = new File(targetPath);
		this.srcPathF = new File(srcPath);
	}

	public ComponentContainer getContainer() {
		return this.container;
	}

	public StrolchAgent getAgent() {
		return this.agent;
	}

	public PrivilegeHandler getPrivilegeHandler() {
		return this.container.getPrivilegeHandler();
	}

	public ServiceHandler getServiceHandler() {
		return this.container.getComponent(ServiceHandler.class);
	}

	public StrolchRealm getRealm(String realm) {
		return this.container.getRealm(realm);
	}

	public RuntimeMock mockRuntime() {

		if (!this.targetPathF.getParentFile().getName().equals(TARGET)) {
			String msg = "Mocking path must be in a maven target: {0}";
			msg = MessageFormat.format(msg, this.targetPathF.getAbsolutePath());
			throw new RuntimeException(msg);
		}

		File configSrc = new File(this.srcPathF, StrolchBootstrapper.PATH_CONFIG);

		if (!configSrc.isDirectory() || !configSrc.canRead()) {
			String msg = "Could not find config source in: {0}";
			msg = MessageFormat.format(msg, configSrc.getAbsolutePath());
			throw new RuntimeException(msg);
		}

		if (this.targetPathF.exists()) {
			logger.info("Deleting all files in {}", this.targetPathF.getAbsolutePath());
			if (!FileHelper.deleteFile(this.targetPathF, true)) {
				String msg = "Failed to delete {0}";
				msg = MessageFormat.format(msg, this.targetPathF.getAbsolutePath());
				throw new RuntimeException(msg);
			}
		}

		if (!this.targetPathF.mkdirs()) {
			String msg = "Failed to create {0}";
			msg = MessageFormat.format(msg, this.targetPathF.getAbsolutePath());
			throw new RuntimeException(msg);
		}

		logger.info("Mocking runtime from {} to {}", this.srcPathF.getAbsolutePath(),
				this.targetPathF.getAbsolutePath());

		// setup the container
		this.agent = new StrolchBootstrapper(getAppVersion()).setupByCopyingRoot("dev", this.srcPathF,
				this.targetPathF);

		return this;
	}

	public RuntimeMock startContainer() {
		return startContainer("dev");
	}

	public RuntimeMock startContainer(String environment) {

		try {
			this.agent.initialize();
			this.agent.start();
			this.container = this.agent.getContainer();

		} catch (Exception e) {
			logger.error("Failed to start mocked container due to: {}", e.getMessage(), e);
			destroyRuntime();
			throw e;
		}

		return this;
	}

	public RuntimeMock destroyRuntime() {

		if (this.agent == null)
			return this;

		try {
			this.agent.stop();
		} catch (Exception e) {
			logger.info("Failed to stop container: {}", e.getMessage());
		}

		try {
			this.agent.destroy();
		} catch (Exception e) {
			logger.info("Failed to destroy container: {}", e.getMessage());
		}

		return this;
	}

	public void run(StrolchRunnable runnable) throws Exception {
		runnable.run(getAgent());
	}

	@Override
	public void close() throws RuntimeException {
		destroyRuntime();
	}

	public static void assertServiceResult(ServiceResultState expectedState, Class<?> expectedResultType,
			ServiceResult result) {
		assertEquals("Expected service result of type " + expectedResultType + " but was " + result.getClass(),
				expectedResultType, result.getClass());

		if (!expectedState.equals(result.getState())) {
			fail("Expected service result state " + expectedState + " but was " + result.getState() + ": Reason: "
					+ StringHelper.formatException(result.getThrowable()));
		}
	}

	public static void runInStrolch(String targetPath, String srcPath, StrolchRunnable runnable) throws Exception {
		try (RuntimeMock runtimeMock = new RuntimeMock(targetPath, srcPath)) {
			runtimeMock.mockRuntime();
			runtimeMock.startContainer();
			runtimeMock.run(runnable);
		}
	}

	public static StrolchVersion getAppVersion() {
		Properties properties = new Properties();
		try (InputStream in = RuntimeMock.class.getResourceAsStream(StrolchAgent.AGENT_VERSION_PROPERTIES)) {
			properties.load(in);
		} catch (Exception e) {
			throw new IllegalStateException("Failed to read " + StrolchAgent.AGENT_VERSION_PROPERTIES, e);
		}
		return new StrolchVersion(properties);
	}

	public interface StrolchRunnable {

		void run(StrolchAgent agent) throws Exception;
	}
}
