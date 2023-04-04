package li.strolch.utils;

import java.text.MessageFormat;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

import li.strolch.utils.collections.TypedTuple;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.exceptions.ElementLockingException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ElementLockingHandler<T> {
	private static final Logger logger = LoggerFactory.getLogger(ElementLockingHandler.class);
	private final TimeUnit tryLockTimeUnit;
	private final long tryLockTime;
	private final Map<T, TypedTuple<Lock, Long>> lockMap;
	private final ScheduledExecutorService executorService;

	private ScheduledFuture<?> cleanupTask;

	public ElementLockingHandler(ScheduledExecutorService executorService, TimeUnit tryLockTimeUnit, long tryLockTime) {
		DBC.PRE.assertNotNull("TimeUnit must be set!", tryLockTimeUnit);
		DBC.PRE.assertNotEquals("try lock time must not be 0", 0, tryLockTime);

		this.executorService = executorService;
		this.tryLockTimeUnit = tryLockTimeUnit;
		this.tryLockTime = tryLockTime;
		this.lockMap = new ConcurrentHashMap<>();
	}

	/**
	 * Locks the given element by creating a {@link ReentrantLock} on it. If the lock is already held by the calling
	 * thread, then the lock count is increased
	 *
	 * @param element
	 * 		the element for which a {@link ReentrantLock} is to be created and/or locked
	 *
	 * @throws ElementLockingException
	 * 		if the lock could not be acquired
	 */
	public void lock(T element) throws ElementLockingException {
		TypedTuple<Lock, Long> tuple = this.lockMap.computeIfAbsent(element,
				l -> new TypedTuple<>(new Lock(true), System.currentTimeMillis()));
		lock(this.tryLockTimeUnit, this.tryLockTime, tuple, element);
	}

	/**
	 * <p>
	 * Unlocks the given element. This method might not completely unlock the element if a lock counter is used and the
	 * object was locked multiple times.
	 * </p>
	 *
	 * <p>
	 * If the lock must be completely released, then use {@link #releaseLock(T)}
	 * </p>
	 *
	 * @param element
	 * 		the element for which the current/last {@link ReentrantLock} is to be unlocked
	 *
	 * @throws ElementLockingException
	 * 		if unlocking failed
	 */
	public void unlock(T element) throws ElementLockingException {
		TypedTuple<Lock, Long> tuple = this.lockMap.get(element);
		Lock lock = tuple.getFirst();
		if (lock == null || !lock.isHeldByCurrentThread()) {
			logger.error(MessageFormat.format("Trying to unlock not locked element {0}", element));
		} else {
			unlock(lock);
		}
	}

	/**
	 * Releases the lock on the given element, by unlocking all locks, i.e. after this method is called, no lock will be
	 * held anymore by the current thread
	 *
	 * @param element
	 * 		the element for which the {@link ReentrantLock} is to be released
	 *
	 * @throws ElementLockingException
	 * 		if the lock could not be released
	 */
	public void releaseLock(T element) throws ElementLockingException {
		TypedTuple<Lock, Long> tuple = this.lockMap.get(element);
		Lock lock = tuple.getFirst();
		if (lock == null) {
			logger.error(MessageFormat.format("Trying to unlock not locked element {0}", element));
		} else if (!lock.isHeldByCurrentThread()) {
			if (lock.isLocked())
				logger.error(MessageFormat.format("Lock not held by this thread for element {0}", element));
			else
				logger.error(MessageFormat.format("Element {0} is not locked!", element));
		} else {
			releaseLock(lock);
		}
	}

	/**
	 * @see java.util.concurrent.locks.ReentrantLock#tryLock(long, TimeUnit)
	 */
	private void lock(TimeUnit timeUnit, long tryLockTime, TypedTuple<Lock, Long> tuple, T element)
			throws ElementLockingException {
		try {

			Lock lock = tuple.getFirst();
			if (!lock.tryLock() && !lock.tryLock(tryLockTime, timeUnit)) {
				String msg = "Thread {0} failed to acquire lock after {1}s for {2}";
				msg = MessageFormat.format(msg, Thread.currentThread().getName(), timeUnit.toSeconds(tryLockTime),
						element);

				Thread owner = lock.getOwner();
				if (owner == null) {
					logger.error(MessageFormat.format("Lock {0} is currently held by unknown thread!", element));
					logger.error(lock.toString());
				} else {
					Exception e = new Exception();
					e.setStackTrace(owner.getStackTrace());
					logger.error(MessageFormat.format("Lock {0} is currently held by {1}", element, owner), e);
				}

				logger.error("Threads waiting on this lock are:");
				for (Thread queuedThread : lock.getQueuedThreads()) {
					Exception e = new Exception();
					e.setStackTrace(queuedThread.getStackTrace());
					logger.error("\n" + queuedThread.getName(), e);
				}

				throw new ElementLockingException(msg);
			}

			tuple.setSecond(System.currentTimeMillis());

		} catch (InterruptedException e) {
			throw new ElementLockingException("Interrupted while trying to acquire lock for " + element, e);
		}
	}

	/**
	 * @see java.util.concurrent.locks.ReentrantLock#unlock()
	 */
	private void unlock(ReentrantLock lock) throws ElementLockingException {
		try {
			lock.unlock();
		} catch (IllegalMonitorStateException e) {
			throw new ElementLockingException("IllegalMonitorStateException when trying to unlock: " + e.getMessage(),
					e);
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

	/**
	 * Starts this locking handler, executing a task to clear old locks
	 */
	public void start() {
		this.cleanupTask = this.executorService.scheduleAtFixedRate(this::cleanupOldLocks, 1, 1, TimeUnit.HOURS);
	}

	/**
	 * Stops this locking handler, stopping the clearing task
	 */
	public void stop() {
		if (this.cleanupTask != null)
			this.cleanupTask.cancel(true);
	}

	private void cleanupOldLocks() {

		Map<T, TypedTuple<Lock, Long>> lockMap;
		synchronized (this.lockMap) {
			lockMap = new HashMap<>(this.lockMap);
		}

		long maxAge = System.currentTimeMillis() - TimeUnit.HOURS.toMillis(1);
		long count = 0;
		for (Map.Entry<T, TypedTuple<Lock, Long>> entry : lockMap.entrySet()) {
			if (!entry.getValue().getFirst().isLocked() && entry.getValue().getSecond() <= maxAge) {
				this.lockMap.remove(entry.getKey());
				count++;
			}
		}

		logger.info("Pruned " + count + " T locks.");
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
