/*
 * Copyright (c) 2025 Robert von Burg <eitch@eitchnet.ch>
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

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.Assert.*;

public class ElementLockingHandlerTest {

	private ScheduledExecutorService executorService;
	private ElementLockingHandler<String> lockingHandler;

	@Before
	public void setup() {
		this.executorService = Executors.newScheduledThreadPool(4);
		this.lockingHandler = new ElementLockingHandler<>(this.executorService, TimeUnit.SECONDS, 2);
		this.lockingHandler.start();
	}

	@After
	public void tearDown() {
		this.lockingHandler.stop();
		this.executorService.shutdownNow();
	}

	@Test
	public void testBasicLockUnlock() {
		String element = "testElement";

		// Test lock and unlock
		this.lockingHandler.lock(element);
		this.lockingHandler.unlock(element);

		// Should be able to lock again
		this.lockingHandler.lock(element);
		this.lockingHandler.unlock(element);
	}

	@Test
	public void testLockedExecute() {
		String element = "testElement";
		AtomicBoolean actionExecuted = new AtomicBoolean(false);

		this.lockingHandler.lockedExecute(element, () -> actionExecuted.set(true));

		assertTrue("Action should have been executed", actionExecuted.get());
	}

	@Test
	public void testLockedExecuteWithResult() {
		String element = "testElement";
		String result = this.lockingHandler.lockedExecuteWithResult(element, () -> "result");

		assertEquals("Result should be returned from action", "result", result);
	}

	@Test
	public void testReentrantLock() {
		String element = "testElement";

		// First lock
		this.lockingHandler.lock(element);

		// Should be able to lock again (reentrant)
		this.lockingHandler.lock(element);

		// Unlock twice
		this.lockingHandler.unlock(element);
		this.lockingHandler.unlock(element);
	}

	@Test
	public void testReleaseLock() {
		String element = "testElement";

		// Multiple locks
		this.lockingHandler.lock(element);
		this.lockingHandler.lock(element);
		this.lockingHandler.lock(element);

		// Should release all locks at once
		this.lockingHandler.releaseLock(element);

		// Should be able to lock again
		this.lockingHandler.lock(element);
		this.lockingHandler.unlock(element);
	}

	@Test
	public void testConcurrentAccess() throws Exception {
		String element = "sharedElement";
		int threadCount = 5;

		try (ExecutorService testExecutor = Executors.newFixedThreadPool(threadCount)) {
			CountDownLatch startLatch = new CountDownLatch(1);
			CountDownLatch finishLatch = new CountDownLatch(threadCount);
			AtomicInteger concurrentAccessCount = new AtomicInteger(0);
			AtomicInteger maxConcurrentAccess = new AtomicInteger(0);

			for (int i = 0; i < threadCount; i++) {
				testExecutor.submit(() -> {
					try {
						// Wait for signal to start
						startLatch.await();

						this.lockingHandler.lockedExecute(element, () -> {
							int current = concurrentAccessCount.incrementAndGet();
							maxConcurrentAccess.updateAndGet(max -> Math.max(max, current));

							// Simulate some work
							try {
								Thread.sleep(50);
							} catch (InterruptedException e) {
								Thread.currentThread().interrupt();
							}

							concurrentAccessCount.decrementAndGet();
						});

						finishLatch.countDown();
					} catch (InterruptedException e) {
						Thread.currentThread().interrupt();
					}
				});
			}

			// Start all threads
			startLatch.countDown();

			// Wait for all threads to complete
			if (!finishLatch.await(10, TimeUnit.SECONDS)) {
				fail("Not all threads completed in time");
			}

			testExecutor.shutdownNow();

			assertEquals("Only one thread should access the critical section at a time", 1, maxConcurrentAccess.get());
		}
	}

	@Test
	public void testLockTimeout() {

		// Using a short timeout to not delay the test
		ElementLockingHandler<String> shortTimeoutHandler = new ElementLockingHandler<>(this.executorService,
				TimeUnit.MILLISECONDS, 200);

		String element = "testElement";
		CountDownLatch lockReleaseLatch = new CountDownLatch(1);

		// Lock element in a separate thread and hold it
		Thread lockingThread = new Thread(() -> {
			shortTimeoutHandler.lock(element);
			try {
				// Hold lock until signaled
				lockReleaseLatch.await();
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
			} finally {
				shortTimeoutHandler.unlock(element);
			}
		});

		lockingThread.start();

		// Give time for the lock to be acquired
		try {
			Thread.sleep(100);
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}

		// Try to acquire the lock from main thread - should throw exception after timeout
		assertThrows(ElementLockingException.class, () -> shortTimeoutHandler.lock(element));

		// Clean up
		lockReleaseLatch.countDown();
		try {
			lockingThread.join(1000);
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}
	}

	@Test
	public void testMultipleElementLocks() {
		String element1 = "element1";
		String element2 = "element2";

		// Should be able to lock different elements concurrently
		this.lockingHandler.lock(element1);
		this.lockingHandler.lock(element2);

		// Release in any order
		this.lockingHandler.unlock(element2);
		this.lockingHandler.unlock(element1);
	}

	@Test
	public void testExceptionHandlingInLockedExecute() {
		String element = "testElement";

		// Action throws runtime exception
		RuntimeException runtimeException = new RuntimeException("Test exception");
		assertThrows(RuntimeException.class, () -> this.lockingHandler.lockedExecute(element, () -> {
			throw runtimeException;
		}));

		// Action throws checked exception - should be wrapped
		Exception checkedException = new Exception("Checked exception");
		assertThrows(IllegalStateException.class, () -> this.lockingHandler.lockedExecute(element, () -> {
			throw checkedException;
		}));

		assertNotSame("Checked exception should have been wrapped", checkedException.getCause(), runtimeException);

		// Element should be unlocked even after exception
		this.lockingHandler.lock(element);
		this.lockingHandler.unlock(element);
	}

	@Test
	public void testParallelLockedExecution() throws Exception {
		int elementCount = 20;
		int threadCount = 10;
		List<String> elements = new ArrayList<>();
		for (int i = 0; i < elementCount; i++) {
			elements.add("element" + i);
		}

		try (ExecutorService testExecutor = Executors.newFixedThreadPool(threadCount)) {
			CountDownLatch startLatch = new CountDownLatch(1);
			CountDownLatch finishLatch = new CountDownLatch(threadCount * elementCount);
			ConcurrentHashMap<String, AtomicInteger> concurrentAccessMap = new ConcurrentHashMap<>();

			// Initialize the counters
			for (String element : elements) {
				concurrentAccessMap.put(element, new AtomicInteger(0));
			}

			// Create a task for each element to be executed by the thread pool
			for (int i = 0; i < threadCount; i++) {
				for (String element : elements) {
					final String threadId = "Thread-" + i;
					testExecutor.submit(() -> {
						try {
							startLatch.await();

							this.lockingHandler.lockedExecute(element, () -> {
								AtomicInteger counter = concurrentAccessMap.get(element);
								int current = counter.incrementAndGet();
								assertEquals("Element " + element + " accessed concurrently from " + threadId, 1,
										current);

								// Simulate work
								try {
									Thread.sleep(10);
								} catch (InterruptedException e) {
									Thread.currentThread().interrupt();
								}

								counter.decrementAndGet();
							});

							finishLatch.countDown();
						} catch (InterruptedException e) {
							Thread.currentThread().interrupt();
						}
					});
				}
			}

			startLatch.countDown(); // Start all threads
			assertTrue("Not all tasks completed in time", finishLatch.await(20, TimeUnit.SECONDS));

			testExecutor.shutdownNow();

			// Verify no elements are still locked
			for (String element : elements) {
				assertEquals("Element " + element + " should have counter reset to 0", 0,
						concurrentAccessMap.get(element).get());
			}
		}
	}

	@Test
	public void testLockWithRetries() {
		String element = "testElement";

		// Test with explicit zero retries (same as default lock method)
		this.lockingHandler.lock(element, 0);
		this.lockingHandler.unlock(element);

		// Test with positive retries value
		this.lockingHandler.lock(element, 3);
		this.lockingHandler.unlock(element);
	}

	@Test
	public void testLockWithRetriesSucceedsAfterDelay() throws Exception {
		String element = "testElement";
		int retries = 3;

		// Create a handler with very short timeouts for faster testing
		ElementLockingHandler<String> shortTimeoutHandler = new ElementLockingHandler<>(this.executorService,
				TimeUnit.MILLISECONDS, 100);

		CountDownLatch lockAcquiredLatch = new CountDownLatch(1);
		CountDownLatch lockReleaseLatch = new CountDownLatch(1);

		// Thread 1: Lock the element and hold it for a short time
		Thread lockingThread = new Thread(() -> {
			shortTimeoutHandler.lock(element);
			try {
				// Signal that lock is acquired
				lockAcquiredLatch.countDown();
				// Hold the lock for a short time
				Thread.sleep(300);
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
			} finally {
				shortTimeoutHandler.unlock(element);
				// Signal that lock is released
				lockReleaseLatch.countDown();
			}
		});

		lockingThread.start();

		// Wait until the first thread has acquired the lock
		assertTrue("Lock should be acquired by first thread", lockAcquiredLatch.await(1, TimeUnit.SECONDS));

		// Thread 2: Try to lock with retries - should succeed after first thread releases
		Thread retryingThread = new Thread(() -> {
			try {
				// This should retry until it succeeds
				shortTimeoutHandler.lock(element, retries);
				shortTimeoutHandler.unlock(element);
			} catch (ElementLockingException e) {
				fail("Should not throw exception with retries: " + e.getMessage());
			}
		});

		retryingThread.start();

		// Wait for locks to be released
		assertTrue("Locks should be released", lockReleaseLatch.await(2, TimeUnit.SECONDS));

		// Wait for the retrying thread to complete
		retryingThread.join(1000);

		// Both threads should have completed successfully
		assertFalse("First thread should have completed", lockingThread.isAlive());
		assertFalse("Retrying thread should have completed", retryingThread.isAlive());
	}

	@Test
	public void testLockWithRetriesFailsAfterMaxAttempts() throws Exception {
		String element = "testElement";
		int retries = 2; // Allow 2 retries

		// Create a handler with very short timeouts for faster testing
		ElementLockingHandler<String> shortTimeoutHandler = new ElementLockingHandler<>(this.executorService,
				TimeUnit.MILLISECONDS, 50);

		CountDownLatch lockAcquiredLatch = new CountDownLatch(1);
		CountDownLatch testCompletedLatch = new CountDownLatch(1);

		// Thread 1: Lock the element and hold it for longer than retries would allow
		Thread lockingThread = new Thread(() -> {
			shortTimeoutHandler.lock(element);
			try {
				// Signal that lock is acquired
				lockAcquiredLatch.countDown();
				// Hold the lock longer than all retry attempts would take
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
			} finally {
				shortTimeoutHandler.unlock(element);
			}
		});

		lockingThread.start();

		// Wait until the first thread has acquired the lock
		assertTrue("Lock should be acquired by first thread", lockAcquiredLatch.await(1, TimeUnit.SECONDS));

		// Thread 2: Try to lock with limited retries - should fail after max attempts
		Thread retryingThread = new Thread(() -> {
			try {
				// This should retry but eventually fail
				shortTimeoutHandler.lock(element, retries);
				fail("Should have thrown ElementLockingException after max retries");
			} catch (ElementLockingException e) {
				// Expected exception
				assertTrue("Exception message should mention failure to acquire lock",
						e.getMessage().contains("failed to acquire lock"));
			} finally {
				testCompletedLatch.countDown();
			}
		});

		retryingThread.start();

		// Wait for the test to complete
		assertTrue("Test should complete", testCompletedLatch.await(5, TimeUnit.SECONDS));

		// Clean up
		lockingThread.interrupt();
		lockingThread.join(1000);
	}

	@Test
	public void testLockWithNegativeRetries() {
		String element = "testElement";

		// Negative retries should be treated as zero retries
		this.lockingHandler.lock(element, -1);
		this.lockingHandler.unlock(element);
	}

	@Test
	public void testLockedExecuteWithCustomRetries() {
		String element = "testElement";
		AtomicBoolean executed = new AtomicBoolean(false);

		this.lockingHandler.lockedExecute(element, () -> executed.set(true), 3);

		assertTrue("Action should have been executed", executed.get());
	}

	@Test
	public void testLockPerformanceWithRetries() {
		String element = "testElement";
		int iterations = 1000;

		// Measure performance with no retries
		long startTime = System.nanoTime();
		for (int i = 0; i < iterations; i++) {
			this.lockingHandler.lock(element, 0);
			this.lockingHandler.unlock(element);
		}
		long timeWithoutRetries = System.nanoTime() - startTime;

		// Measure performance with retries specified (but not needed)
		startTime = System.nanoTime();
		for (int i = 0; i < iterations; i++) {
			this.lockingHandler.lock(element, 3);
			this.lockingHandler.unlock(element);
		}
		long timeWithRetries = System.nanoTime() - startTime;

		// The performance should be similar as no retries are actually performed
		// This is mostly to ensure there's no significant overhead
		double ratio = (double) timeWithRetries / timeWithoutRetries;
		assertTrue("Performance with retries specified should be reasonably close to "
				+ "performance without retries: ratio = "
				+ ratio, ratio < 1.5);
	}
}