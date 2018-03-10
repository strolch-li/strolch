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
package li.strolch.agent.impl;

import java.text.MessageFormat;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

import li.strolch.agent.api.LockHandler;
import li.strolch.agent.api.StrolchLockException;
import li.strolch.model.Locator;
import li.strolch.utils.dbc.DBC;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultLockHandler implements LockHandler {

	private static final Logger logger = LoggerFactory.getLogger(DefaultLockHandler.class);

	private String realm;
	private TimeUnit tryLockTimeUnit;
	private long tryLockTime;
	private Map<Locator, ReentrantLock> lockMap;

	public DefaultLockHandler(String realm, TimeUnit tryLockTimeUnit, long tryLockTime) {

		DBC.PRE.assertNotEmpty("Realm must be set!", realm); //$NON-NLS-1$
		DBC.PRE.assertNotNull("TimeUnit must be set!", tryLockTimeUnit); //$NON-NLS-1$
		DBC.PRE.assertNotEquals("try lock time must not be 0", 0, tryLockTime); //$NON-NLS-1$

		this.realm = realm;
		this.tryLockTimeUnit = tryLockTimeUnit;
		this.tryLockTime = tryLockTime;
		this.lockMap = new ConcurrentHashMap<>();
	}

	public String getRealm() {
		return this.realm;
	}

	@Override
	public void lock(Locator locator) throws StrolchLockException {
		ReentrantLock lock = this.lockMap.computeIfAbsent(locator, l -> new ReentrantLock(true));
		lock(this.tryLockTimeUnit, this.tryLockTime, lock, locator);
	}

	@Override
	public void unlock(Locator locator) throws StrolchLockException {
		ReentrantLock lock = this.lockMap.get(locator);
		if (lock == null || !lock.isHeldByCurrentThread()) {
			logger.error(MessageFormat.format("Trying to unlock not locked element {0}", locator)); //$NON-NLS-1$
		} else {
			// logger.debug("unlocking " + locator); //$NON-NLS-1$
			unlock(lock);
		}
	}

	@Override
	public void releaseLock(Locator locator) throws StrolchLockException {
		ReentrantLock lock = this.lockMap.get(locator);
		if (lock == null) {
			logger.error(MessageFormat.format("Trying to unlock not locked element {0}", locator));
		} else if (!lock.isHeldByCurrentThread()) {
			if (lock.isLocked())
				logger.error(MessageFormat.format("Lock not held by this thread for element {0}", locator));
			else
				logger.error(MessageFormat.format("Element {0} is not locked!", locator));
		} else {
			//logger.info("releasing lock " + locator); //$NON-NLS-1$
			releaseLock(lock);
		}
	}

	/**
	 * @see java.util.concurrent.locks.ReentrantLock#tryLock(long, TimeUnit)
	 */
	private void lock(TimeUnit timeUnit, long tryLockTime, ReentrantLock lock, Locator locator)
			throws StrolchLockException {
		try {

			if (!lock.tryLock(tryLockTime, timeUnit)) {
				String msg = "Failed to acquire lock after {0}s for {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, timeUnit.toSeconds(tryLockTime), locator);
				throw new StrolchLockException(msg);
			}

			// logger.debug("locked " + locator); //$NON-NLS-1$

		} catch (InterruptedException e) {
			throw new StrolchLockException(e.getMessage(), e); //$NON-NLS-1$
		}
	}

	/**
	 * @see java.util.concurrent.locks.ReentrantLock#unlock()
	 */
	private void unlock(ReentrantLock lock) throws StrolchLockException {
		try {
			lock.unlock();
		} catch (IllegalMonitorStateException e) {
			throw new StrolchLockException(e.getMessage(), e); //$NON-NLS-1$
		}
	}

	/**
	 * @see java.util.concurrent.locks.ReentrantLock#unlock()
	 */
	private void releaseLock(ReentrantLock lock) {
		while (lock.isHeldByCurrentThread() && lock.isLocked()) {
			unlock(lock);
		}
	}
}
