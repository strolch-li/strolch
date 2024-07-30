package li.strolch.utils.concurrent;

import static java.lang.Thread.currentThread;
import static java.text.MessageFormat.format;

import java.util.Collection;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
		try {

			if (!tryLock() && !tryLock(tryLockTime, timeUnit)) {
				String msg = "Thread {0} failed to acquire lock after {1}s for {2}";
				msg = format(msg, currentThread().getName(), timeUnit.toSeconds(tryLockTime), this.name);

				Thread owner = getOwner();
				if (owner == null) {
					logger.error(format("Lock {0} is currently held by unknown thread!", this.name));
					logger.error(toString());
				} else {
					Exception e = new Exception();
					e.setStackTrace(owner.getStackTrace());
					logger.error(format("Lock {0} is currently held by {1}", this.name, owner), e);
				}

				logger.error("Threads waiting on this lock are:");
				for (Thread queuedThread : getQueuedThreads()) {
					Exception e = new Exception();
					e.setStackTrace(queuedThread.getStackTrace());
					logger.error("\n{}", queuedThread.getName(), e);
				}

				throw new ElementLockingException(msg);
			}

			this.lastLockTime = System.currentTimeMillis();

		} catch (InterruptedException e) {
			throw new ElementLockingException("Interrupted while trying to acquire lock for " + this.name, e);
		}
	}
}
