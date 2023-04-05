package li.strolch.utils.concurrent;

import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

import li.strolch.utils.CheckedRunnable;
import li.strolch.utils.CheckedSupplier;
import li.strolch.utils.collections.TypedTuple;
import li.strolch.utils.dbc.DBC;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ElementLockingHandler<T> {
	private static final Logger logger = LoggerFactory.getLogger(ElementLockingHandler.class);
	private final TimeUnit tryLockTimeUnit;
	private final long tryLockTime;
	private final Map<T, TypedTuple<ElementLock, Long>> lockMap;
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
	 * First locks the given element, then calls the given action, using a try catch/finally to unlock the element after
	 * the action has completed.
	 * <p>
	 * Note that only {@link #lock(Object)} and {@link #unlock(Object)} are called, if the element was locked
	 * previously, then the lock counter is only reduced to the value prior to the call
	 *
	 * @param element
	 * 		the element to lock
	 * @param action
	 * 		the action to perform
	 */
	public void lockedExecute(T element, CheckedRunnable action) {
		lock(element);
		try {
			action.run();
		} catch (RuntimeException e) {
			logger.error("Failed to execute action " + action + " for locked element " + element, e);
			throw e;
		} catch (Exception e) {
			throw new IllegalStateException("Failed to execute action " + action + " for locked element " + element, e);
		} finally {
			unlock(element);
		}
	}

	/**
	 * First locks the given element, then calls the given action, returning any result, using a try catch/finally to
	 * unlock the element after the action has completed.
	 * <p>
	 * Note that only {@link #lock(Object)} and {@link #unlock(Object)} are called, if the element was locked
	 * previously, then the lock counter is only reduced to the value prior to the call
	 *
	 * @param element
	 * 		the element to lock
	 * @param action
	 * 		the action to perform
	 *
	 * @return the result of the action
	 */
	public <U> U lockedExecuteWithResult(T element, CheckedSupplier<U> action) {
		lock(element);
		try {
			return action.get();
		} catch (RuntimeException e) {
			logger.error("Failed to execute action " + action + " for locked element " + element, e);
			throw e;
		} catch (Exception e) {
			throw new IllegalStateException("Failed to execute action " + action + " for locked element " + element, e);
		} finally {
			unlock(element);
		}
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
		DBC.PRE.assertNotNull("element may not be null!", element);
		TypedTuple<ElementLock, Long> tuple = this.lockMap.computeIfAbsent(element, this::newLock);
		tuple.getFirst().lock(this.tryLockTimeUnit, this.tryLockTime);
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
		DBC.PRE.assertNotNull("element may not be null!", element);
		TypedTuple<ElementLock, Long> tuple = this.lockMap.get(element);
		ElementLock elementLock = tuple.getFirst();
		if (elementLock == null || !elementLock.isHeldByCurrentThread()) {
			logger.error(MessageFormat.format("Trying to unlock not locked element {0}", element));
		} else {
			unlock(elementLock);
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
		DBC.PRE.assertNotNull("element may not be null!", element);
		TypedTuple<ElementLock, Long> tuple = this.lockMap.get(element);
		ElementLock elementLock = tuple.getFirst();
		if (elementLock == null) {
			logger.error(MessageFormat.format("Trying to unlock not locked element {0}", element));
		} else if (!elementLock.isHeldByCurrentThread()) {
			if (elementLock.isLocked())
				logger.error(MessageFormat.format("Lock not held by this thread for element {0}", element));
			else
				logger.error(MessageFormat.format("Element {0} is not locked!", element));
		} else {
			releaseLock(elementLock);
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

		Map<T, TypedTuple<ElementLock, Long>> lockMap;
		synchronized (this.lockMap) {
			lockMap = new HashMap<>(this.lockMap);
		}

		long maxAge = System.currentTimeMillis() - TimeUnit.HOURS.toMillis(1);
		long count = 0;
		for (Map.Entry<T, TypedTuple<ElementLock, Long>> entry : lockMap.entrySet()) {
			if (!entry.getValue().getFirst().isLocked() && entry.getValue().getSecond() <= maxAge) {
				this.lockMap.remove(entry.getKey());
				count++;
			}
		}

		logger.info("Pruned " + count + " T locks.");
	}

	private TypedTuple<ElementLock, Long> newLock(T element) {
		return new TypedTuple<>(new ElementLock(element.toString(), true), 0L);
	}
}
