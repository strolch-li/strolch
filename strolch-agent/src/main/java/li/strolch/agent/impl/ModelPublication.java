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
package li.strolch.agent.impl;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.StampedLock;
import java.util.function.Supplier;

/** Short memory-publication barrier; persistence proceeds concurrently except administrative bulk clears. */
public final class ModelPublication {
	private final StampedLock memory = new StampedLock();
	private final StampedLock persistence = new StampedLock();
	private volatile Throwable failure;
	private volatile AffectedRoots publishing;

	@FunctionalInterface
	interface AffectedRoots {
		boolean contains(String interpretation, String type, String id);
	}

	/**
	 * Read the root first, then inspect the active publication. ConcurrentHashMap root reads acquire the
	 * publication writes, so a newly visible root cannot bypass its preceding affected-root announcement.
	 * An unrelated publication cannot change this root and needs no retry or shared-lock acquisition.
	 */
	<T> T readRoot(String interpretation, String type, String id, Supplier<T> read) {
		assertAvailable();
		T value = read.get();
		AffectedRoots active = this.publishing;
		if (active != null && active.contains(interpretation, type, id)) {
			long stamp = this.memory.readLock();
			try { assertAvailable(); value = read.get(); }
			finally { this.memory.unlockRead(stamp); }
		}
		assertAvailable();
		return value;
	}

	public <T> T read(Supplier<T> read) {
		assertAvailable();
		long stamp = this.memory.tryOptimisticRead();
		T value = read.get();
		if (!this.memory.validate(stamp)) {
			stamp = this.memory.readLock();
			try {
				assertAvailable();
				value = read.get();
			} finally {
				this.memory.unlockRead(stamp);
			}
		}
		assertAvailable();
		return value;
	}

	public Lock persistenceLock(boolean bulkClear) {
		return bulkClear ? this.persistence.asWriteLock() : this.persistence.asReadLock();
	}

	public void publish(Runnable preparedChanges) {
		publish(preparedChanges, (_, _, _) -> true);
	}

	// A partial publication must fence reads even for an Error; always rethrow the original failure.
	@SuppressWarnings("java:S1181")
	void publish(Runnable preparedChanges, AffectedRoots affected) {
		long stamp = this.memory.writeLock();
		try {
			assertAvailable();
			this.publishing = affected;
			preparedChanges.run();
		} catch (RuntimeException | Error e) {
			fail(e);
			throw e;
		} finally {
			this.publishing = null;
			this.memory.unlockWrite(stamp);
		}
	}

	public void fail(Throwable cause) {
		this.failure = cause;
	}

	public void assertAvailable() {
		if (this.failure != null)
			throw new IllegalStateException("Realm model is unavailable; reload committed persistence before serving data", this.failure);
	}
}
