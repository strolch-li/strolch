/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
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

		this.waitForWorkersTime = LockableObject.getLockTime() + (long) ((double) this.getWaitForWorkersTime() * .2);
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
			String workerName = resourceId + "_" + i;
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
