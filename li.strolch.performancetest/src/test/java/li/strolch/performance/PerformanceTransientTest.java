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
package li.strolch.performance;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;

import li.strolch.privilege.model.Certificate;
import li.strolch.service.api.ServiceHandler;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PerformanceTransientTest extends PerformanceTest {

	public static final String RUNTIME_PATH = "target/runtime_transient_test/"; //$NON-NLS-1$
	public static final String CONFIG_SRC = "src/runtime_transient"; //$NON-NLS-1$

	@BeforeClass
	public static void beforeClass() throws Exception {
		buildRuntime(CONFIG_SRC, RUNTIME_PATH);
	}

	@AfterClass
	public static void afterClass() throws Exception {
		afterClass(RUNTIME_PATH);
	}

	@Test
	public void runPerformanceTest() {
		Certificate certificate = runtime().getPrivilegeHandler().authenticate("transient", "transient".toCharArray());
		ServiceHandler svcHandler = runtime().getServiceHandler();
		svcHandler.doService(certificate, new PerformanceTestService(), argInstance());
	}

	@Test
	@Ignore
	public void runParallelPerformanceTest() {

		int nrOfTasks = 5;

		ForkJoinPool commonPool = ForkJoinPool.commonPool();

		long start = System.currentTimeMillis();
		List<ForkJoinTask<Long>> tasks = new ArrayList<>();
		for (int i = 0; i < nrOfTasks; i++) {
			PerformanceTask task = new PerformanceTask();
			tasks.add(task);
			commonPool.execute(task);
		}

		logger.info("Executing " + tasks.size() + " tasks...");

		List<Long> results = new ArrayList<>();
		for (ForkJoinTask<Long> task : tasks) {
			results.add(task.join());
		}
		logger.info("Executed " + tasks.size() + " tasks.");
		for (int i = 0; i < results.size(); i++) {
			logger.info("Task " + i + " executed " + results.get(i) + " TXs");
		}

		long avg = (long) results.stream().mapToLong(l -> l).average().getAsDouble();
		long took = System.currentTimeMillis() - start;
		long txPerSec = avg / (took / 1000);
		logger.info("Average TXs was " + avg + " with " + txPerSec + " TXs/s");
	}

	public class PerformanceTask extends ForkJoinTask<Long> {

		private long nrOfTxs;

		@Override
		public Long getRawResult() {
			return this.nrOfTxs;
		}

		@Override
		protected void setRawResult(Long value) {
			// ignore
		}

		@Override
		protected boolean exec() {

			Certificate certificate = runtime().getPrivilegeHandler()
					.authenticate("transient", "transient".toCharArray());
			ServiceHandler svcHandler = runtime().getServiceHandler();
			PerformanceTestResult svcResult = svcHandler
					.doService(certificate, new PerformanceTestService(), new PerformanceTestArgument());
			runtime().getPrivilegeHandler().invalidate(certificate);

			this.nrOfTxs = svcResult.getNrOfTxs();

			return true;
		}
	}
}
