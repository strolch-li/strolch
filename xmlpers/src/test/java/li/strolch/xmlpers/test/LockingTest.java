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
package li.strolch.xmlpers.test;

import static li.strolch.xmlpers.test.impl.TestConstants.TYPE_RES;
import static li.strolch.xmlpers.test.model.ModelBuilder.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import li.strolch.xmlpers.api.IoMode;
import li.strolch.xmlpers.api.PersistenceConstants;
import li.strolch.xmlpers.api.PersistenceTransaction;
import li.strolch.xmlpers.objref.IdOfSubTypeRef;
import li.strolch.xmlpers.objref.LockableObject;
import li.strolch.xmlpers.test.model.MyModel;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class LockingTest extends AbstractPersistenceTest {

	private static final String BASE_PATH = "target/db/LockingTest/";

	private long waitForWorkersTime;
	private boolean run;

	@BeforeClass
	public static void beforeClass() {
		cleanPath(BASE_PATH);
	}

	@Before
	public void before() {
		Properties properties = new Properties();
		properties.setProperty(PersistenceConstants.PROP_XML_IO_MOD, IoMode.DOM.name());
		properties.setProperty(PersistenceConstants.PROP_BASEPATH, BASE_PATH + IoMode.DOM.name());
		properties.setProperty(PersistenceConstants.PROP_LOCK_TIME_MILLIS, Long.toString(500L));
		properties.setProperty(PersistenceConstants.PROP_ALLOW_OVERWRITE_ON_CREATE, Boolean.FALSE.toString());
		setup(properties);

		this.waitForWorkersTime = LockableObject.getLockTime() + 300L;
	}

	@Test
	public void shouldLockObjects() throws InterruptedException {

		List<CreateResourceWorker> workers = new ArrayList<>(5);

		String resoureId = "worker";
		for (int i = 0; i < 5; i++) {
			String workerName = resoureId + "_" + i;
			CreateResourceWorker worker = new CreateResourceWorker(workerName, workerName);
			worker.start();
			workers.add(worker);
			logger.info("Setup thread " + worker.getName());
		}

		int nrOfSuccess = runWorkers(workers);

		assertEquals("Only one thread should be able to perform the TX!", 5, nrOfSuccess);
	}

	@Test
	public void shouldFailIfResourceAlreadyExists() throws InterruptedException {

		List<CreateResourceWorker> workers = new ArrayList<>(5);

		String resourceId = "createWorkerRes";
		for (int i = 0; i < 5; i++) {
			String workerName = resourceId + "_" + i;
			CreateResourceWorker worker = new CreateResourceWorker(workerName, resourceId);
			worker.start();
			workers.add(worker);
			logger.info("Setup thread " + worker.getName());
		}

		int nrOfSuccess = runWorkers(workers);

		assertEquals("Only one thread should be able to perform the TX!", 1, nrOfSuccess);
	}

	@Test
	public void shouldFailUpdateIfLockNotAcquirable() throws InterruptedException {

		// prepare workers
		List<UpdateResourceWorker> workers = new ArrayList<>(5);
		String resourceId = "updatWorkerRes";
		for (int i = 0; i < 5; i++) {
			String workerName = resourceId + "_" + i;
			UpdateResourceWorker worker = new UpdateResourceWorker(workerName, resourceId);
			worker.start();
			workers.add(worker);
			logger.info("Setup thread " + worker.getName());
		}

		int nrOfSuccess;
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {

			// create resource which is to be updated
			MyModel resource = createResource(resourceId);
			tx.getObjectDao().add(resource);

			// and before closing the TX, run the workers, which should fail as we are still holding the locks
			nrOfSuccess = runWorkers(workers);
		}

		assertEquals("Only one thread should be able to perform the TX!", 0, nrOfSuccess);
	}

	private int runWorkers(List<? extends AbstractWorker> workers) throws InterruptedException {

		setRun(true);

		for (AbstractWorker worker : workers) {
			worker.join(getWaitForWorkersTime() + 5000L);
		}

		int nrOfSuccess = 0;
		for (AbstractWorker worker : workers) {
			if (worker.isSuccess())
				nrOfSuccess++;
		}

		return nrOfSuccess;
	}

	public long getWaitForWorkersTime() {
		return this.waitForWorkersTime;
	}

	public boolean isRun() {
		return this.run;
	}

	public void setRun(boolean run) {
		this.run = run;
	}

	public abstract class AbstractWorker extends Thread {

		protected boolean success;
		protected String resourceId;

		public AbstractWorker(String name, String resourceId) {
			super(name);
			this.resourceId = resourceId;
		}

		@Override
		public void run() {

			logger.info("Waiting for ok to work...");
			while (!isRun()) {
				try {
					Thread.sleep(10L);
				} catch (InterruptedException e) {
					throw new RuntimeException(e);
				}
			}

			logger.info("Starting work...");
			try (PersistenceTransaction tx = LockingTest.this.persistenceManager.openTx()) {
				doWork(tx);
			}

			this.success = true;
			logger.info("Work completed.");
		}

		protected abstract void doWork(PersistenceTransaction tx);

		public boolean isSuccess() {
			return this.success;
		}
	}

	public class CreateResourceWorker extends AbstractWorker {

		public CreateResourceWorker(String name, String resourceId) {
			super(name, resourceId);
		}

		@Override
		protected void doWork(PersistenceTransaction tx) {
			MyModel resource = createResource(this.resourceId);
			tx.getObjectDao().add(resource);
		}
	}

	public class UpdateResourceWorker extends AbstractWorker {

		public UpdateResourceWorker(String name, String resourceId) {
			super(name, resourceId);
		}

		@Override
		protected void doWork(PersistenceTransaction tx) {

			IdOfSubTypeRef objectRef = tx.getManager().getObjectRefCache()
					.getIdOfSubTypeRef(TYPE_RES, RES_TYPE, this.resourceId);
			MyModel resource = tx.getObjectDao().queryById(objectRef);
			assertNotNull(resource);
			updateResource(resource);

			tx.getObjectDao().update(resource);
		}
	}
}
