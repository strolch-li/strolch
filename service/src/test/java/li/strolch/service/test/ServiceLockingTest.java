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
package li.strolch.service.test;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import li.strolch.model.Locator;
import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.service.api.*;
import li.strolch.testbase.runtime.RuntimeMock;
import org.hamcrest.MatcherAssert;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ServiceLockingTest {

	private static final String RUNTIME_PATH = "target/lockingTest/";
	private static final String CONFIG_SRC = "src/test/resources/transienttest";

	private static final String RESOURCE_LOCATOR = "Resource/TestType/MyTestResource";

	protected static RuntimeMock runtimeMock;
	private volatile boolean run;

	@BeforeClass
	public static void beforeClass() {

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		runtimeMock.startContainer();
	}

	@AfterClass
	public static void afterClass() {
		runtimeMock.destroyRuntime();
	}

	public static ServiceHandler getServiceHandler() {
		return runtimeMock.getContainer().getComponent(ServiceHandler.class);
	}

	public static Certificate login() {
		return runtimeMock.getPrivilegeHandler().authenticate("test", "test".toCharArray());
	}

	@Test
	public void shouldLockElements() throws InterruptedException {

		List<LockingRunner> runners = new ArrayList<>();

		// create the long running service
		LockingServiceTest longRunningService = new LockingServiceTest();
		LockingArgumentTest longRunningArg = new LockingArgumentTest();
		longRunningArg.longRunning = true;
		longRunningArg.resourceLoc = Locator.valueOf(RESOURCE_LOCATOR);
		runners.add(new LockingRunner(longRunningService, longRunningArg));

		// create multiple services which try and modify the same service, but are not long running
		for (int i = 0; i < 5; i++) {
			LockingServiceTest svc = new LockingServiceTest();
			LockingArgumentTest arg = new LockingArgumentTest();
			arg.longRunning = false;
			arg.resourceLoc = Locator.valueOf(RESOURCE_LOCATOR);
			runners.add(new LockingRunner(svc, arg));
		}

		runRunners(runners);

		// now assert that we can perform another such service, thus validating that the resource is not locked any longer
		doLockServiceTest();
	}

	private void runRunners(List<LockingRunner> runners) throws InterruptedException {
		this.run = false;
		for (LockingRunner lockingRunner : runners) {
			lockingRunner.start();
		}
		this.run = true;

		for (LockingRunner lockingRunner : runners) {
			lockingRunner.join();
		}

		assertNotNull(runners.get(0).getResult());
		assertEquals(ServiceResultState.SUCCESS, runners.get(0).getResult().getState());
		for (int i = 1; i < runners.size(); i++) {
			ServiceResult result = runners.get(i).getResult();
			assertEquals(ServiceResultState.EXCEPTION, result.getState());
			MatcherAssert.assertThat(result.getMessage(), containsString("failed to acquire lock after"));
		}
	}

	@Test
	public void shouldUnlockCompletelyOnMultipleLock() throws InterruptedException {

		List<LockingRunner> runners = new ArrayList<>();

		LockingServiceTest svc = new LockingServiceTest();
		LockingArgumentTest arg = new LockingArgumentTest();
		arg.longRunning = false;
		arg.nrOfLocks = 5;
		arg.resourceLoc = Locator.valueOf(RESOURCE_LOCATOR);
		runners.add(new LockingRunner(svc, arg));

		runRunners(runners);

		// now assert that we can perform another such service, thus validating that the resource is not locked any longer
		doLockServiceTest();
	}

	private void doLockServiceTest() {
		LockingServiceTest svc = new LockingServiceTest();
		LockingArgumentTest arg = new LockingArgumentTest();
		arg.longRunning = false;
		arg.resourceLoc = Locator.valueOf(RESOURCE_LOCATOR);
		ServiceResult result = getServiceHandler().doService(login(), svc, arg);
		assertEquals(ServiceResultState.SUCCESS, result.getState());
	}

	private class LockingRunner extends Thread {

		private final LockingServiceTest svc;
		private final LockingArgumentTest arg;

		private ServiceResult result;

		public LockingRunner(LockingServiceTest svc, LockingArgumentTest arg) {
			super();
			this.svc = svc;
			this.arg = arg;
		}

		@Override
		public void run() {

			while (!ServiceLockingTest.this.run) {
				// spin lock
			}

			this.result = getServiceHandler().doService(login(), this.svc, this.arg);
		}

		public ServiceResult getResult() {
			return this.result;
		}
	}

	private static class LockingArgumentTest extends ServiceArgument {

		public boolean longRunning;
		public int nrOfLocks = 1;
		public Locator resourceLoc;
	}

	private static class LockingServiceTest extends AbstractService<LockingArgumentTest, ServiceResult> {

		@Override
		protected ServiceResult getResultInstance() {
			return new ServiceResult();
		}

		@Override
		public LockingArgumentTest getArgumentInstance() {
			return new LockingArgumentTest();
		}

		@Override
		protected ServiceResult internalDoService(LockingArgumentTest arg) throws Exception {

			try (StrolchTransaction tx = openArgOrUserTx(arg)) {

				if (!arg.longRunning)
					Thread.sleep(500L);

				Resource res = tx.findElement(arg.resourceLoc);
				for (int i = 0; i < arg.nrOfLocks; i++)
					tx.lock(res);

				if (arg.longRunning)
					Thread.sleep(5000L);

				tx.commitOnClose();
			}

			return ServiceResult.success();
		}
	}
}
