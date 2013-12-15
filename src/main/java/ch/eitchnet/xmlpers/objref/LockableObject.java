package ch.eitchnet.xmlpers.objref;

import java.text.MessageFormat;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.xmlpers.api.XmlPersistenceException;

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
				msg = MessageFormat.format(msg, StringHelper.formatMillisecondsDuration(tryLockTime), this.toString());
				throw new XmlPersistenceException(msg);
			}
			logger.info("locked " + this.toString()); //$NON-NLS-1$
		} catch (InterruptedException e) {
			throw new XmlPersistenceException("Thread interrupted: " + e.getMessage(), e); //$NON-NLS-1$
		}
	}

	/**
	 * @see java.util.concurrent.locks.ReentrantLock#unlock()
	 */
	public void unlock() {
		this.lock.unlock();
		logger.info("unlocking " + this.toString()); //$NON-NLS-1$
	}
}
