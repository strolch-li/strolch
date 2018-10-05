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

import java.text.MessageFormat;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

import li.strolch.utils.helper.StringHelper;
import li.strolch.xmlpers.api.XmlPersistenceException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LockableObject {

	private static final Logger logger = LoggerFactory.getLogger(LockableObject.class);
	private static long tryLockTime = 10000L;

	private final ReentrantLock lock;

	public LockableObject() {
		this.lock = new ReentrantLock(true);
	}

	public static long getLockTime() {
		return tryLockTime;
	}

	/**
	 * @see java.util.concurrent.locks.ReentrantLock#tryLock(long, TimeUnit)
	 */
	public void lock() {
		try {

			if (!this.lock.tryLock(tryLockTime, TimeUnit.MILLISECONDS)) {
				String msg = "Failed to acquire lock after {0} for {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, StringHelper.formatMillisecondsDuration(tryLockTime), toString());
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
	public void unlock() {
		this.lock.unlock();
		if (logger.isDebugEnabled())
			logger.debug("unlocking " + toString()); //$NON-NLS-1$
	}
}
