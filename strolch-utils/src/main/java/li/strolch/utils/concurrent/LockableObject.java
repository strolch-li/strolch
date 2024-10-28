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
package li.strolch.utils.concurrent;

import java.util.concurrent.TimeUnit;

public class LockableObject {

	private static long tryLockTime = 10000L;

	public static void setTryLockTime(long tryLockTime) {
		LockableObject.tryLockTime = tryLockTime;
	}

	private final ElementLock elementLock;
	protected final String name;

	public LockableObject(String name) {
		this.name = name;
		this.elementLock = new ElementLock(name, true);
	}

	public String getName() {
		return this.name;
	}

	public static long getLockTime() {
		return tryLockTime;
	}

	public void lock() {

		// don't lock multiple times
		if (this.elementLock.isHeldByCurrentThread() && this.elementLock.isLocked())
			return;

		this.elementLock.lock(TimeUnit.MILLISECONDS, tryLockTime);
	}

	/**
	 * @see java.util.concurrent.locks.ReentrantLock#unlock()
	 */
	public void releaseLock() {
		while (this.elementLock.isHeldByCurrentThread() && this.elementLock.isLocked()) {
			this.elementLock.unlock();
		}
	}
}
