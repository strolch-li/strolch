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
package li.strolch.xmlpers.objref;

import static java.lang.Thread.currentThread;
import static java.text.MessageFormat.format;
import static li.strolch.utils.helper.StringHelper.formatMillisecondsDuration;

import java.text.MessageFormat;
import java.util.Collection;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

import li.strolch.xmlpers.api.XmlPersistenceException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LockableObject {

	private static final Logger logger = LoggerFactory.getLogger(LockableObject.class);
	private static long tryLockTime = 10000L;

	public static void setTryLockTime(long tryLockTime) {
		LockableObject.tryLockTime = tryLockTime;
	}

	private final Lock lock;
	protected final String name;

	public LockableObject(String name) {
		this.name = name;
		this.lock = new Lock(true);
	}

	public String getName() {
		return this.name;
	}

	public static long getLockTime() {
		return tryLockTime;
	}

	/**
	 * @see java.util.concurrent.locks.ReentrantLock#tryLock(long, TimeUnit)
	 */
	public void lock() {

		// don't lock multiple times
		if (this.lock.isHeldByCurrentThread() && this.lock.isLocked())
			return;

		try {

			if (!this.lock.tryLock(tryLockTime, TimeUnit.MILLISECONDS)) {
				String msg = "Thread {0} failed to acquire lock after {1} for {2}"; //$NON-NLS-1$
				msg = format(msg, currentThread().getName(), formatMillisecondsDuration(tryLockTime), this);

				Thread owner = lock.getOwner();
				if (owner == null) {
					logger.error(MessageFormat.format("Lock {0} is currently held unknown thread!", this.name));
					logger.error(lock.toString());
				} else {
					Exception e = new Exception();
					e.setStackTrace(owner.getStackTrace());
					logger.error(MessageFormat.format("Lock {0} is currently held by {1}", this.name, owner), e);
				}

				logger.error("Threads waiting on this lock are:");
				for (Thread queuedThread : lock.getQueuedThreads()) {
					Exception e = new Exception();
					e.setStackTrace(queuedThread.getStackTrace());
					logger.error("\n" + queuedThread.getName(), e);
				}

				throw new XmlPersistenceException(msg);
			}
			if (logger.isDebugEnabled())
				logger.debug("locked " + toString()); //$NON-NLS-1$
		} catch (InterruptedException e) {
			throw new XmlPersistenceException("Thread interrupted: " + e.getMessage(), e); //$NON-NLS-1$
		}
	}

	/**
	 * @see java.util.concurrent.locks.ReentrantLock#unlock()
	 */
	public void releaseLock() {
		while (this.lock.isHeldByCurrentThread() && this.lock.isLocked()) {
			if (logger.isDebugEnabled())
				logger.debug("unlocking " + toString()); //$NON-NLS-1$
			this.lock.unlock();
		}
	}

	public static class Lock extends ReentrantLock {

		public Lock(boolean fair) {
			super(fair);
		}

		@Override
		public Thread getOwner() {
			return super.getOwner();
		}

		@Override
		public Collection<Thread> getQueuedThreads() {
			return super.getQueuedThreads();
		}
	}
}
