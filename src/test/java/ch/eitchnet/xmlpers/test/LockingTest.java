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

import static ch.eitchnet.xmlpers.test.model.ModelBuilder.createResource;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import ch.eitchnet.xmlpers.api.IoMode;
import ch.eitchnet.xmlpers.api.PersistenceConstants;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import ch.eitchnet.xmlpers.objref.LockableObject;
import ch.eitchnet.xmlpers.test.model.Resource;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class LockingTest extends AbstractPersistenceTest {

	private static final String BASE_PATH = "target/db/LockingTest/"; //$NON-NLS-1$

	long waitForWorkersTime = LockableObject.getLockTime() + 2000L;

	@BeforeClass
	public static void beforeClass() {
		cleanPath(BASE_PATH);
	}

	@Before
	public void before() {
		Properties properties = new Properties();
		properties.setProperty(PersistenceConstants.PROP_BASEPATH, BASE_PATH + IoMode.DOM.name());
		setup(properties);
	}

	@Test
	public void shouldLockObjects() throws InterruptedException {

		List<Worker> workers = new ArrayList<>(5);

		for (int i = 0; i < 5; i++) {

			String workerName = "worker_" + i; //$NON-NLS-1$
			Worker worker = new Worker(workerName, workerName);
			worker.start();
			workers.add(worker);
			logger.info("Setup thread " + worker.getName()); //$NON-NLS-1$
		}

		int nrOfSuccess = runWorkers(workers);

		assertEquals("Only one thread should be able to perform the TX!", 5, nrOfSuccess); //$NON-NLS-1$
	}

	@Test
	public void shouldFailIfLockNotAcquirable() throws InterruptedException {

		List<Worker> workers = new ArrayList<>(5);

		for (int i = 0; i < 5; i++) {

			String workerName = "workerRes"; //$NON-NLS-1$
			Worker worker = new Worker(workerName, workerName);
			worker.start();
			workers.add(worker);
			logger.info("Setup thread " + worker.getName()); //$NON-NLS-1$
		}

		int nrOfSuccess = runWorkers(workers);

		assertEquals("Only one thread should be able to perform the TX!", 1, nrOfSuccess); //$NON-NLS-1$
	}

	private int runWorkers(List<Worker> workers) throws InterruptedException {

		synchronized (this) {
			this.notifyAll();
		}

		for (Worker worker : workers) {
			worker.join(this.waitForWorkersTime + 2000L);
		}

		int nrOfSuccess = 0;
		for (Worker worker : workers) {
			if (worker.isSuccess())
				nrOfSuccess++;
		}

		return nrOfSuccess;
	}

	public class Worker extends Thread {

		private boolean success;
		private String resourceId;

		public Worker(String name, String resourceId) {
			super(name);
			this.resourceId = resourceId;
		}

		public void run() {

			synchronized (LockingTest.this) {
				try {
					logger.info("Waiting for ok to work..."); //$NON-NLS-1$
					LockingTest.this.wait();
				} catch (InterruptedException e) {
					throw new RuntimeException(e);
				}
			}

			logger.info("Starting work..."); //$NON-NLS-1$
			try (PersistenceTransaction tx = LockingTest.this.persistenceManager.openTx()) {

				Resource resource = createResource(this.resourceId);
				tx.getObjectDao().add(resource);
			}
			this.success = true;

			try {
				Thread.sleep(LockingTest.this.waitForWorkersTime);
			} catch (InterruptedException e) {
				throw new RuntimeException(e);
			}

			logger.info("Work completed."); //$NON-NLS-1$
		}

		public boolean isSuccess() {
			return this.success;
		}
	}
}
