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
package li.strolch.testbase.runtime;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.InputStream;
import java.text.MessageFormat;
import java.util.Properties;

import li.strolch.agent.api.*;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.runtime.privilege.PrivilegedRunnable;
import li.strolch.runtime.privilege.PrivilegedRunnableWithResult;
import li.strolch.service.api.*;
import li.strolch.utils.helper.FileHelper;
import li.strolch.utils.helper.StringHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RuntimeMock {

	private static final Logger logger = LoggerFactory.getLogger(RuntimeMock.class);
	private static final String TARGET = "target";

	private ComponentContainer container;
	private StrolchAgent agent;

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

	public <T> T getComponent(Class<T> clazz) {
		return this.container.getComponent(clazz);
	}

	public StrolchRealm getRealm(String realm) {
		return this.container.getRealm(realm);
	}

	public StrolchTransaction openUserTx(Certificate certificate, boolean readOnly) {
		return this.container.getRealm(certificate).openTx(certificate, getClass(), readOnly);
	}

	public Certificate loginAdmin() {
		return getPrivilegeHandler().authenticate("admin", "admin".toCharArray());
	}

	public Certificate loginTest() {
		return getPrivilegeHandler().authenticate("test", "test".toCharArray());
	}

	public Certificate login(String username, String password) {
		return getPrivilegeHandler().authenticate(username, password.toCharArray());
	}

	public boolean logout(Certificate cert) {
		return getPrivilegeHandler().invalidate(cert);
	}

	public RuntimeMock mockRuntime(String targetPath, String srcPath) {
		File targetPathF = new File(targetPath);
		File srcPathF = new File(srcPath);
		return mockRuntime(targetPathF, srcPathF);
	}

	public RuntimeMock mockRuntime(File targetPathF, File srcPathF) {

		if (!targetPathF.getParentFile().getName().equals(TARGET)) {
			String msg = "Mocking path must be in a maven target: {0}";
			msg = MessageFormat.format(msg, targetPathF.getAbsolutePath());
			throw new RuntimeException(msg);
		}

		File configSrc = new File(srcPathF, StrolchBootstrapper.PATH_CONFIG);

		if (!configSrc.isDirectory() || !configSrc.canRead()) {
			String msg = "Could not find config source in: {0}";
			msg = MessageFormat.format(msg, configSrc.getAbsolutePath());
			throw new RuntimeException(msg);
		}

		if (targetPathF.exists()) {
			logger.info("Deleting all files in " + targetPathF.getAbsolutePath());
			if (!FileHelper.deleteFile(targetPathF, true)) {
				String msg = "Failed to delete {0}";
				msg = MessageFormat.format(msg, targetPathF.getAbsolutePath());
				throw new RuntimeException(msg);
			}
		}

		if (!targetPathF.mkdirs()) {
			String msg = "Failed to create {0}";
			msg = MessageFormat.format(msg, targetPathF.getAbsolutePath());
			throw new RuntimeException(msg);
		}

		logger.info(
				MessageFormat.format("Mocking runtime from {0} to {1}", srcPathF.getAbsolutePath(),
						targetPathF.getAbsolutePath()));

		// setup the container
		this.agent = new StrolchBootstrapper(getAppVersion()).setupByCopyingRoot("dev", srcPathF, targetPathF);

		return this;
	}

	public void startContainer() {
		startContainer("dev");
	}

	public void startContainer(String environment) {

		try {
			this.agent.initialize();
			this.agent.start();
			this.container = this.agent.getContainer();

		} catch (Exception e) {
			logger.error("Failed to start mocked container due to: " + e.getMessage(), e);
			destroyRuntime();
			throw e;
		}
	}

	public void destroyRuntime() {

		if (this.agent == null)
			return;

		try {
			this.agent.stop();
		} catch (Exception e) {
			logger.info("Failed to stop container: " + e.getMessage());
		}

		try {
			this.agent.destroy();
		} catch (Exception e) {
			logger.info("Failed to destroy container: " + e.getMessage());
		}
	}

	public void runAsAgent(PrivilegedRunnable runnable) throws Exception {
		this.agent.runAsAgent(runnable);
	}

	public <T> T runAsAgentWithResult(PrivilegedRunnableWithResult<T> runnable) throws Exception {
		return this.agent.runAsAgentWithResult(runnable);
	}

	public <T extends ServiceArgument, U extends ServiceResult> U doService(Certificate certificate,
			Service<T, U> service, T argument) {
		return getServiceHandler().doService(certificate, service, argument);
	}

	public <T extends ServiceArgument, U extends ServiceResult> U doServiceAssertResult(Certificate certificate,
			Service<T, U> service, T argument) {
		U result = getServiceHandler().doService(certificate, service, argument);
		assertServiceResult(ServiceResultState.SUCCESS, result);
		return result;
	}

	public <T extends ServiceArgument, U extends ServiceResult> U doServiceAssertResultFailed(Certificate certificate,
			Service<T, U> service, T argument) {
		U result = getServiceHandler().doService(certificate, service, argument);
		assertServiceResult(ServiceResultState.FAILED, result);
		return result;
	}

	public static void assertServiceResult(ServiceResultState expectedState, Class<?> expectedResultType,
			ServiceResult result) {
		assertEquals("Expected service result of type " + expectedResultType + " but was " + result.getClass(),
				expectedResultType, result.getClass());

		if (!expectedState.equals(result.getState())) {
			String errorMsg;
			if (result.getThrowable() == null)
				errorMsg = result.getMessage();
			else
				errorMsg = StringHelper.formatException(result.getThrowable());

			fail("Expected service result state " + expectedState + " but was " + result.getState() + ": Reason: "
					+ errorMsg);
		}
	}

	public static void assertServiceResult(ServiceResultState expectedState, ServiceResult result) {
		if (!expectedState.equals(result.getState())) {
			String errorMsg;
			if (result.getThrowable() == null)
				errorMsg = result.getMessage();
			else
				errorMsg = StringHelper.formatException(result.getThrowable());

			fail("Expected service result state " + expectedState + " but was " + result.getState() + ": Reason: "
					+ errorMsg);
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
}
