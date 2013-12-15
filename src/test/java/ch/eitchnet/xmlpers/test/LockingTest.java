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
package ch.eitchnet.xmlpers.test;

import static ch.eitchnet.xmlpers.test.impl.TestConstants.TYPE_RES;
import static ch.eitchnet.xmlpers.test.model.ModelBuilder.RES_TYPE;
import static ch.eitchnet.xmlpers.test.model.ModelBuilder.createResource;
import static ch.eitchnet.xmlpers.test.model.ModelBuilder.updateResource;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import ch.eitchnet.xmlpers.api.IoMode;
import ch.eitchnet.xmlpers.api.PersistenceConstants;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import ch.eitchnet.xmlpers.objref.IdOfSubTypeRef;
import ch.eitchnet.xmlpers.objref.LockableObject;
import ch.eitchnet.xmlpers.test.model.Resource;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class LockingTest extends AbstractPersistenceTest {

	private static final String BASE_PATH = "target/db/LockingTest/"; //$NON-NLS-1$

	private long waitForWorkersTime;
	private boolean run;

	@BeforeClass
	public static void beforeClass() {
		cleanPath(BASE_PATH);
	}

	@Before
	public void before() {
		Properties properties = new Properties();
		properties.setProperty(PersistenceConstants.PROP_BASEPATH, BASE_PATH + IoMode.DOM.name());
		properties.setProperty(PersistenceConstants.PROP_LOCK_TIME_MILLIS, Long.toString(500L));
		setup(properties);

		this.waitForWorkersTime = LockableObject.getLockTime() + this.getWaitForWorkersTime() + 300L;
	}

	@Test
	public void shouldLockObjects() throws InterruptedException {

		List<CreateResourceWorker> workers = new ArrayList<>(5);

		String resoureId = "worker"; //$NON-NLS-1$
		for (int i = 0; i < 5; i++) {
			String workerName = resoureId + "_" + i; //$NON-NLS-1$
			CreateResourceWorker worker = new CreateResourceWorker(workerName, workerName);
			worker.start();
			workers.add(worker);
			logger.info("Setup thread " + worker.getName()); //$NON-NLS-1$
		}

		int nrOfSuccess = runWorkers(workers);

		assertEquals("Only one thread should be able to perform the TX!", 5, nrOfSuccess); //$NON-NLS-1$
	}

	@Test
	public void shouldFailIfResourceAlreadyExists() throws InterruptedException {

		List<CreateResourceWorker> workers = new ArrayList<>(5);

		String resourceId = "createWorkerRes"; //$NON-NLS-1$
		for (int i = 0; i < 5; i++) {
			String workerName = resourceId + "_" + i; //$NON-NLS-1$
			CreateResourceWorker worker = new CreateResourceWorker(workerName, resourceId);
			worker.start();
			workers.add(worker);
			logger.info("Setup thread " + worker.getName()); //$NON-NLS-1$
		}

		int nrOfSuccess = runWorkers(workers);

		assertEquals("Only one thread should be able to perform the TX!", 1, nrOfSuccess); //$NON-NLS-1$
	}

	@Test
	public void shouldFailUpdateIfLockNotAcquirable() throws InterruptedException {

		// prepare workers
		List<UpdateResourceWorker> workers = new ArrayList<>(5);
		String resourceId = "updatWorkerRes"; //$NON-NLS-1$
		for (int i = 0; i < 5; i++) {
			String workerName = resourceId + "_" + i; //$NON-NLS-1$
			UpdateResourceWorker worker = new UpdateResourceWorker(workerName, resourceId);
			worker.start();
			workers.add(worker);
			logger.info("Setup thread " + worker.getName()); //$NON-NLS-1$
		}

		// create resource which is to be updated
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {
			Resource resource = createResource(resourceId);
			tx.getObjectDao().add(resource);
		}

		int nrOfSuccess = runWorkers(workers);

		assertEquals("Only one thread should be able to perform the TX!", 1, nrOfSuccess); //$NON-NLS-1$
	}

	private int runWorkers(List<? extends AbstractWorker> workers) throws InterruptedException {

		this.setRun(true);

		for (AbstractWorker worker : workers) {
			worker.join(this.getWaitForWorkersTime() + 2000L);
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

		public void run() {

			logger.info("Waiting for ok to work..."); //$NON-NLS-1$
			while (!LockingTest.this.isRun()) {
				try {
					Thread.sleep(10L);
				} catch (InterruptedException e) {
					throw new RuntimeException(e);
				}
			}

			logger.info("Starting work..."); //$NON-NLS-1$
			try (PersistenceTransaction tx = LockingTest.this.persistenceManager.openTx()) {
				doWork(tx);

				try {
					Thread.sleep(getWaitForWorkersTime());
				} catch (InterruptedException e) {
					throw new RuntimeException(e);
				}

				this.success = true;
			}

			logger.info("Work completed."); //$NON-NLS-1$
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
			Resource resource = createResource(this.resourceId);
			tx.getObjectDao().add(resource);
		}
	}

	public class UpdateResourceWorker extends AbstractWorker {

		public UpdateResourceWorker(String name, String resourceId) {
			super(name, resourceId);
		}

		@Override
		protected void doWork(PersistenceTransaction tx) {

			IdOfSubTypeRef objectRef = tx.getObjectRefCache().getIdOfSubTypeRef(TYPE_RES, RES_TYPE, this.resourceId);
			Resource resource = tx.getObjectDao().queryById(objectRef);
			assertNotNull(resource);
			updateResource(resource);

			tx.getObjectDao().update(resource);
		}
	}
}
