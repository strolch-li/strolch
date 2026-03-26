/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.utils.concurrent;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

import static java.lang.Thread.currentThread;
import static java.text.MessageFormat.format;

public class ElementLock extends ReentrantLock {

	private static final Logger logger = LoggerFactory.getLogger(ElementLock.class);
	private final String name;
	private long lastLockTime;

	public ElementLock(String name, boolean fair) {
		super(fair);
		this.name = name;
	}

	public String getName() {
		return this.name;
	}

	public long getLastLockTime() {
		return this.lastLockTime;
	}

	@Override
	public Thread getOwner() {
		return super.getOwner();
	}

	@Override
	public Collection<Thread> getQueuedThreads() {
		return super.getQueuedThreads();
	}

	public void lock(TimeUnit timeUnit, long tryLockTime) throws ElementLockingException {
		lock(timeUnit, tryLockTime, 0);
	}

	public void lock(TimeUnit timeUnit, long tryLockTime, int retries) throws ElementLockingException {
		try {

			// first try to acquire the lock immediately
			if (tryLock() || tryLock(tryLockTime, timeUnit)) {
				this.lastLockTime = System.currentTimeMillis();
				return;
			}

			if (retries == 0)
				throw handleLockTimeout(timeUnit, tryLockTime);

			// retry with exponential backoff
			long waitTimeMs = timeUnit.toMillis(tryLockTime) / 10; // Start with 10% of initial lock time
			for (int i = 0; i < retries; i++) {
				logger.warn("Failed to acquire lock for {}. Retrying in {}ms (attempt {}/{})", this.name, waitTimeMs,
						(i + 1), retries);
				handleRetry();

				Thread.sleep(waitTimeMs);

				// Try to acquire the lock again
				if (tryLock() || tryLock(tryLockTime, timeUnit)) {
					this.lastLockTime = System.currentTimeMillis();
					logger.debug("Successfully acquired lock for {} after {} retries", this.name, (i + 1));
					return;
				}

				// Exponential backoff: double the wait time for the next attempt
				waitTimeMs *= 2;
			}

			throw handleLockTimeout(timeUnit, tryLockTime);

		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
			logger.error("Interrupted while trying to acquire lock for {}", this.name);
		}
	}

	protected void handleRetry() {
		// do nothing
	}

	private ElementLockingException handleLockTimeout(TimeUnit timeUnit, long tryLockTime) {
		String msg = "Thread {0} failed to acquire lock after {1}s for {2}";
		msg = format(msg, currentThread().getName(), timeUnit.toSeconds(tryLockTime), this.name);

		Thread owner = getOwner();
		if (owner == null) {
			logger.error("Lock {} is currently held by unknown thread!", this.name);
			logger.error(toString());
		} else {
			Exception e = new Exception();
			e.setStackTrace(owner.getStackTrace());
			logger.error("Lock {} is currently held by {}", this.name, owner, e);
		}

		logger.error("Threads waiting on this lock are:");
		for (Thread queuedThread : getQueuedThreads()) {
			Exception e = new Exception();
			e.setStackTrace(queuedThread.getStackTrace());
			logger.error("\n{}", queuedThread.getName(), e);
		}

		return new ElementLockingException(msg);
	}
}
