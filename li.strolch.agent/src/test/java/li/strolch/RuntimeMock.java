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
import java.text.MessageFormat;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.FileHelper;
import ch.eitchnet.utils.helper.StringHelper;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

/**
 * Basically you should use the RuntimeMock class in the testbase project, but to mitigate circular dependencies, in
 * tests of the agent project we use this implementation
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RuntimeMock implements AutoCloseable {

	private static final Logger logger = LoggerFactory.getLogger(RuntimeMock.class);
	private static final String TARGET = "target"; //$NON-NLS-1$

	private ComponentContainer container;
	private StrolchAgent agent;
	private File targetPathF;
	private File srcPathF;

	public RuntimeMock(String targetPath, String srcPath) {
		this(new File(targetPath), new File(srcPath));
	}

	public RuntimeMock(File targetPathF, File srcPathF) {
		this.targetPathF = targetPathF;
		this.srcPathF = srcPathF;
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
			String msg = "Mocking path must be in a maven target: {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, this.targetPathF.getAbsolutePath());
			throw new RuntimeException(msg);
		}

		File configSrc = new File(this.srcPathF, RuntimeConfiguration.PATH_CONFIG);

		if (!configSrc.isDirectory() || !configSrc.canRead()) {
			String msg = "Could not find config source in: {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, configSrc.getAbsolutePath());
			throw new RuntimeException(msg);
		}

		if (this.targetPathF.exists()) {
			logger.info("Deleting all files in " + this.targetPathF.getAbsolutePath()); //$NON-NLS-1$
			if (!FileHelper.deleteFile(this.targetPathF, true)) {
				String msg = "Failed to delete {0}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, this.targetPathF.getAbsolutePath());
				throw new RuntimeException(msg);
			}
		}

		if (!this.targetPathF.mkdirs()) {
			String msg = "Failed to create {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, this.targetPathF.getAbsolutePath());
			throw new RuntimeException(msg);
		}

		logger.info(MessageFormat.format("Mocking runtime from {0} to {1}", this.srcPathF.getAbsolutePath(), //$NON-NLS-1$
				this.targetPathF.getAbsolutePath()));

		if (!FileHelper.copy(this.srcPathF.listFiles(), this.targetPathF, false)) {
			String msg = "Failed to copy source files from {0} to {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, this.srcPathF.getAbsolutePath(), this.targetPathF.getAbsolutePath());
			throw new RuntimeException(msg);
		}

		return this;
	}

	public RuntimeMock startContainer() {
		return startContainer("dev");
	}

	public RuntimeMock startContainer(String environment) {

		try {
			StrolchAgent agent = new StrolchAgent();
			agent.setup(environment, this.targetPathF);
			agent.initialize();
			agent.start();

			this.agent = agent;
			this.container = agent.getContainer();

		} catch (Exception e) {
			logger.error("Failed to start mocked container due to: " + e.getMessage(), e); //$NON-NLS-1$
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
			logger.info("Failed to stop container: " + e.getMessage()); //$NON-NLS-1$
		}

		try {
			this.agent.destroy();
		} catch (Exception e) {
			logger.info("Failed to destroy container: " + e.getMessage()); //$NON-NLS-1$
		}

		return this;
	}

	@Override
	public void close() throws RuntimeException {
		this.destroyRuntime();
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

	public static void runInStrolch(String targetPath, String srcPath, StrolchRunnable runnable) {
		try (RuntimeMock runtimeMock = new RuntimeMock(targetPath, srcPath)) {
			runtimeMock.mockRuntime();
			runtimeMock.startContainer();

			runnable.run(runtimeMock.getAgent());
		}
	}

	public interface StrolchRunnable {

		public void run(StrolchAgent agent);
	}
}
