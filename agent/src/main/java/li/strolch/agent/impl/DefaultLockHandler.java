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
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

import li.strolch.agent.api.LockHandler;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchLockException;
import li.strolch.model.Locator;
import li.strolch.utils.collections.TypedTuple;
import li.strolch.utils.dbc.DBC;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultLockHandler implements LockHandler {

	private static final Logger logger = LoggerFactory.getLogger(DefaultLockHandler.class);

	private final StrolchAgent agent;
	private final String realm;
	private final TimeUnit tryLockTimeUnit;
	private final long tryLockTime;
	private final Map<Locator, TypedTuple<Lock, Long>> lockMap;

	private ScheduledFuture<?> cleanupTask;

	public DefaultLockHandler(StrolchAgent agent, String realm, TimeUnit tryLockTimeUnit, long tryLockTime) {
		DBC.PRE.assertNotNull("agent must be set!", agent);
		DBC.PRE.assertNotEmpty("Realm must be set!", realm);
		DBC.PRE.assertNotNull("TimeUnit must be set!", tryLockTimeUnit);
		DBC.PRE.assertNotEquals("try lock time must not be 0", 0, tryLockTime);

		this.agent = agent;
		this.realm = realm;
		this.tryLockTimeUnit = tryLockTimeUnit;
		this.tryLockTime = tryLockTime;
		this.lockMap = new ConcurrentHashMap<>();
	}

	@Override
	public void start() {
		this.cleanupTask = agent.getScheduledExecutor()
				.scheduleAtFixedRate(this::cleanupOldLocks, 1, 1, TimeUnit.HOURS);
	}

	@Override
	public void stop() {
		if (this.cleanupTask != null)
			this.cleanupTask.cancel(true);
	}

	private void cleanupOldLocks() {

		Map<Locator, TypedTuple<Lock, Long>> lockMap;
		synchronized (this.lockMap) {
			lockMap = new HashMap<>(this.lockMap);
		}

		long maxAge = System.currentTimeMillis() - TimeUnit.HOURS.toMillis(1);
		long count = 0;
		for (Map.Entry<Locator, TypedTuple<Lock, Long>> entry : lockMap.entrySet()) {
			if (!entry.getValue().getFirst().isLocked() && entry.getValue().getSecond() <= maxAge) {
				this.lockMap.remove(entry.getKey());
				count++;
			}
		}

		logger.info("Pruned " + count + " Locator locks.");
	}

	public String getRealm() {
		return this.realm;
	}

	@Override
	public void lock(Locator locator) throws StrolchLockException {
		TypedTuple<Lock, Long> tuple = this.lockMap.computeIfAbsent(locator,
				l -> new TypedTuple<>(new Lock(true), System.currentTimeMillis()));
		lock(this.tryLockTimeUnit, this.tryLockTime, tuple, locator);
	}

	@Override
	public void unlock(Locator locator) throws StrolchLockException {
		TypedTuple<Lock, Long> tuple = this.lockMap.get(locator);
		Lock lock = tuple.getFirst();
		if (lock == null || !lock.isHeldByCurrentThread()) {
			logger.error(MessageFormat.format("Trying to unlock not locked element {0}", locator));
		} else {
			unlock(lock);
		}
	}

	@Override
	public void releaseLock(Locator locator) throws StrolchLockException {
		TypedTuple<Lock, Long> tuple = this.lockMap.get(locator);
		Lock lock = tuple.getFirst();
		if (lock == null) {
			logger.error(MessageFormat.format("Trying to unlock not locked element {0}", locator));
		} else if (!lock.isHeldByCurrentThread()) {
			if (lock.isLocked())
				logger.error(MessageFormat.format("Lock not held by this thread for element {0}", locator));
			else
				logger.error(MessageFormat.format("Element {0} is not locked!", locator));
		} else {
			releaseLock(lock);
		}
	}

	/**
	 * @see java.util.concurrent.locks.ReentrantLock#tryLock(long, TimeUnit)
	 */
	private void lock(TimeUnit timeUnit, long tryLockTime, TypedTuple<Lock, Long> tuple, Locator locator)
			throws StrolchLockException {
		try {

			Lock lock = tuple.getFirst();
			if (!lock.tryLock() && !lock.tryLock(tryLockTime, timeUnit)) {
				String msg = "Thread {0} failed to acquire lock after {1}s for {2}";
				msg = MessageFormat.format(msg, Thread.currentThread().getName(), timeUnit.toSeconds(tryLockTime),
						locator);

				Thread owner = lock.getOwner();
				if (owner == null) {
					logger.error("Lock is currently held unknown thread!");
					logger.error(lock.toString());
				} else {
					Exception e = new Exception();
					e.setStackTrace(owner.getStackTrace());
					logger.error(MessageFormat.format("Lock is currently held by {0}", owner), e);
				}

				logger.error("Threads waiting on this lock are:");
				for (Thread queuedThread : lock.getQueuedThreads()) {
					Exception e = new Exception();
					e.setStackTrace(queuedThread.getStackTrace());
					logger.error("\n" + queuedThread.getName(), e);
				}

				throw new StrolchLockException(msg);
			}

			tuple.setSecond(System.currentTimeMillis());

		} catch (InterruptedException e) {
			throw new StrolchLockException("Interrupted while trying to acquire lock for " + locator, e);
		}
	}

	/**
	 * @see java.util.concurrent.locks.ReentrantLock#unlock()
	 */
	private void unlock(ReentrantLock lock) throws StrolchLockException {
		try {
			lock.unlock();
		} catch (IllegalMonitorStateException e) {
			throw new StrolchLockException("IllegalMonitorStateException when trying to unlock: " + e.getMessage(), e);
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
