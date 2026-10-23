/*
 * Copyright (c) 2026 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.transaction;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.api.StrolchTransactionException;
import li.strolch.persistence.api.StrolchTransactionConflictException;
import li.strolch.service.api.Command;
import li.strolch.privilege.model.Certificate;
import li.strolch.search.ResourceSearch;
import li.strolch.testbase.runtime.RuntimeMock;
import li.strolch.utils.concurrent.ElementLockingException;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.junit.Assert.*;

/** Regression coverage for transient snapshot isolation and serializable writer validation. */
public class TransientTransactionIsolationTest {

	private final String type = "AcidBaseline-" + UUID.randomUUID();
	private RuntimeMock runtime;
	private Certificate certificate;
	private ExecutorService executor;

	@Before
	public void setUp() throws Exception {
		Path root = Path.of("target", getClass().getSimpleName(), this.type);
		Path source = root.resolve("source");
		Path fixture = Path.of("src/test/resources/transienttest");
		try (var paths = Files.walk(fixture)) {
			for (Path path : paths.toList()) {
				Path target = source.resolve(fixture.relativize(path));
				if (Files.isDirectory(path))
					Files.createDirectories(target);
				else
					Files.copy(path, target);
			}
		}
		this.runtime = new RuntimeMock().mockRuntime(root.resolve("runtime").toFile(), source.toFile());
		this.runtime.startContainer();
		this.certificate = this.runtime.loginTest();
		this.executor = Executors.newFixedThreadPool(2);
		try (StrolchTransaction tx = open(false)) {
			tx.add(new Resource("a", "1", this.type));
			tx.add(new Resource("b", "1", this.type));
			tx.commitOnClose();
		}
	}

	@After
	public void tearDown() throws Exception {
		try {
			if (this.executor != null) {
				this.executor.shutdownNow();
				assertTrue("Test threads did not terminate", this.executor.awaitTermination(15, TimeUnit.SECONDS));
			}
		} finally {
			if (this.runtime != null)
				this.runtime.destroyRuntime();
		}
	}

	private StrolchTransaction open(boolean readOnly) {
		StrolchTransaction tx = this.runtime.getContainer().getRealm(this.certificate)
				.openTx(this.certificate, getClass(), readOnly);
		return readOnly ? tx : tx.rollbackOnFailure();
	}

	private Resource get(StrolchTransaction tx, String id) {
		return tx.getResourceBy(this.type, id, true);
	}

	private void set(StrolchTransaction tx, String id, String value) {
		Resource resource = tx.readLock(get(tx, id));
		resource.setName(value);
		tx.update(resource);
	}

	private String read(String id) {
		try (StrolchTransaction tx = open(true)) {
			return get(tx, id).getName();
		}
	}

	private static void await(CountDownLatch latch) throws InterruptedException {
		assertTrue("Schedule could not complete: timeout is diagnostic, not proof of a safety violation",
				latch.await(10, TimeUnit.SECONDS));
	}

	private static <T> T result(Future<T> future) throws Exception {
		return future.get(15, TimeUnit.SECONDS);
	}

	@Test
	public void uncommittedChangesStayPrivate() throws Exception {
		assertPrivateUntilRollback(false);
	}

	@Test
	public void flushedChangesStayPrivateAndRollBack() throws Exception {
		assertPrivateUntilRollback(true);
	}

	private void assertPrivateUntilRollback(boolean flush) throws Exception {
		CountDownLatch staged = new CountDownLatch(1);
		CountDownLatch finish = new CountDownLatch(1);
		Future<?> writer = this.executor.submit(() -> {
			try (StrolchTransaction tx = open(false)) {
				set(tx, "a", "2");
				if (flush)
					tx.flush();
				assertEquals("2", get(tx, "a").getName());
				staged.countDown();
				await(finish);
				tx.rollbackOnClose();
			}
			return null;
		});
		String observed;
		try {
			await(staged);
			observed = result(this.executor.submit(() -> read("a")));
		} finally {
			finish.countDown();
		}
		result(writer);
		assertEquals("Another transaction saw an uncommitted value", "1", observed);
		assertEquals("Rollback did not restore the committed state", "1", read("a"));
	}

	@Test
	public void rollbackRestoresBothFlushedObjects() {
		// Independent of the privacy assertion, which can fail before rollback is checked.
		try (StrolchTransaction tx = open(false)) {
			set(tx, "a", "2");
			set(tx, "b", "2");
			tx.flush();
			tx.rollbackOnClose();
		}
		assertEquals("1", read("a"));
		assertEquals("1", read("b"));
	}

	@Test
	public void commandFailureAfterFlushRestoresBothObjects() {
		StrolchTransactionException failure = assertThrows(StrolchTransactionException.class, () -> {
			try (StrolchTransaction tx = open(false)) {
				set(tx, "a", "2");
				set(tx, "b", "2");
				tx.flush();
				tx.addCommand(new Command(tx) {
					@Override
					public void validate() {
					}

					@Override
					public void doCommand() {
						throw new IllegalStateException("Injected baseline command failure");
					}
				});
				tx.commitOnClose();
			}
		});
		assertTrue("Must reach the injected command, not fail during setup",
				failure.getCause() instanceof IllegalStateException);
		assertEquals("Injected baseline command failure", failure.getCause().getMessage());
		assertEquals("1", read("a"));
		assertEquals("1", read("b"));
	}

	@Test
	public void readLockRefreshesARetainedCopy() throws Exception {
		try (StrolchTransaction reader = open(false)) {
			Resource retained = get(reader, "a");
			result(this.executor.submit(() -> {
				try (StrolchTransaction writer = open(false)) {
					set(writer, "a", "2");
					writer.commitOnClose();
				}
				return null;
			}));
			assertEquals("Retained copy must remain immutable", "1", retained.getName());
			assertEquals("2", reader.readLock(retained).getName());
			reader.rollbackOnClose();
		}
	}

	@Test
	public void lockRefreshCannotCompleteAMixedRevisionRead() throws Exception {
		StrolchTransactionException failure = assertThrows(StrolchTransactionException.class, () -> {
			try (StrolchTransaction reader = this.runtime.getContainer().getRealm(this.certificate)
					.openTx(this.certificate, getClass(), false)) {
				assertEquals("1", get(reader, "a").getName());
				result(this.executor.submit(() -> {
					try (StrolchTransaction writer = open(false)) {
						set(writer, "a", "2");
						set(writer, "b", "2");
						writer.commitOnClose();
					}
					return null;
				}));
				assertEquals("2", reader.readLock(get(reader, "b")).getName());
			}
		});
		assertTrue(failure.getCause() instanceof StrolchTransactionConflictException);
	}

	@Test
	public void transactionStartingAfterCommitSeesIt() throws Exception {
		result(this.executor.submit(() -> {
			try (StrolchTransaction tx = open(false)) {
				set(tx, "a", "2");
				tx.commitOnClose();
			}
			return null;
		}));
		assertEquals("2", result(this.executor.submit(() -> read("a"))));
	}

	@Test
	public void readerCannotCommitAFracturedPair() throws Exception {
		// Reader starts before writer and reads a=1. Writer atomically writes a=b=2. Reader must not finish with b=2.
		String first;
		String second;
		try (StrolchTransaction reader = open(true)) {
			first = get(reader, "a").getName();
			result(this.executor.submit(() -> {
				try (StrolchTransaction writer = open(false)) {
					set(writer, "a", "2");
					set(writer, "b", "2");
					writer.commitOnClose();
				}
				return null;
			}));
			second = get(reader, "b").getName();
		}
		assertEquals("No serial order explains the completed reader's pair", first, second);
	}

	@Test
	public void disjointWriteLocksMustNotPermitWriteSkew() throws Exception {
		CountDownLatch bothRead = new CountDownLatch(2);
		Future<?> first = this.executor.submit(() -> disableIfOtherEnabled("a", "b", bothRead));
		Future<?> second = this.executor.submit(() -> disableIfOtherEnabled("b", "a", bothRead));
		boolean firstCommitted = committed(first);
		boolean secondCommitted = committed(second);
		assertTrue("One writer must make progress", firstCommitted || secondCommitted);
		assertFalse("Both stale writers must not commit", firstCommitted && secondCommitted);
		assertFalse("Both committed after reading the other as enabled; no serial execution permits this",
				read("a").equals("0") && read("b").equals("0"));
	}

	private boolean committed(Future<?> future) throws Exception {
		try {
			result(future);
			return true;
		} catch (ExecutionException e) {
			Throwable cause = e.getCause();
			while (cause != null) {
				if (cause instanceof StrolchTransactionConflictException)
					return false;
				cause = cause.getCause();
			}
			throw e;
		}
	}

	private Void disableIfOtherEnabled(String own, String other, CountDownLatch bothRead) throws InterruptedException {
		try (StrolchTransaction tx = open(false)) {
			boolean enabled = get(tx, other).getName().equals("1");
			bothRead.countDown();
			await(bothRead);
			if (enabled)
				set(tx, own, "0");
			tx.commitOnClose();
		}
		return null;
	}

	@Test
	public void repeatedSearchCannotObserveAPhantom() throws Exception {
		int before;
		int after;
		try (StrolchTransaction reader = open(true)) {
			before = new ResourceSearch().types(this.type).search(reader).toList().size();
			result(this.executor.submit(() -> {
				try (StrolchTransaction writer = open(false)) {
					writer.add(new Resource("c", "1", this.type));
					writer.commitOnClose();
				}
				return null;
			}));
			after = new ResourceSearch().types(this.type).search(reader).toList().size();
		}
		assertEquals("A completed reader must observe a serializable predicate result", before, after);
	}

	@Test
	public void searchSeesOwnInsert() {
		try (StrolchTransaction tx = open(false)) {
			tx.add(new Resource("c", "1", this.type));
			assertTrue(new ResourceSearch().types(this.type).search(tx).toList().stream()
					.anyMatch(resource -> resource.getId().equals("c")));
			tx.rollbackOnClose();
		}
	}

	private void addOtherRoots() {
		try (StrolchTransaction tx = open(false)) {
			tx.add(new Order("o", "1", this.type));
			tx.add(new Activity("x", "1", this.type, TimeOrdering.SERIES));
			tx.commitOnClose();
		}
	}

	private void assertRootNames(StrolchTransaction tx, String expected) {
		assertEquals(expected, get(tx, "a").getName());
		assertEquals(expected, tx.getOrderBy(this.type, "o", true).getName());
		assertEquals(expected, tx.getActivityBy(this.type, "x", true).getName());
	}

	@Test
	public void mixedRootsPublishTogetherAfterCommandExecution() throws Exception {
		addOtherRoots();
		CountDownLatch partial = new CountDownLatch(1);
		CountDownLatch finish = new CountDownLatch(1);
		Future<?> writer = this.executor.submit(() -> {
			try (StrolchTransaction tx = open(false)) {
				tx.addCommand(new Command(tx) {
					@Override
					public void validate() {
					}

					@Override
					public void doCommand() {
						Resource resource = tx.readLock(get(tx, "a"));
						resource.setName("2");
						tx.getResourceMap().update(tx, resource);
						partial.countDown();
						try {
							await(finish);
						} catch (InterruptedException e) {
							Thread.currentThread().interrupt();
							throw new IllegalStateException(e);
						}
						Order order = tx.readLock(tx.getOrderBy(type, "o", true));
						order.setName("2");
						tx.getOrderMap().update(tx, order);
						Activity activity = tx.readLock(tx.getActivityBy(type, "x", true));
						activity.setName("2");
						tx.getActivityMap().update(tx, activity);
					}
				});
				tx.commitOnClose();
			}
			return null;
		});
		try {
			await(partial);
			try (StrolchTransaction reader = open(true)) {
				assertRootNames(reader, "1");
				finish.countDown();
				result(writer);
				assertRootNames(reader, "1");
			}
		} finally {
			finish.countDown();
		}
		try (StrolchTransaction reader = open(true)) {
			assertRootNames(reader, "2");
		}
	}

	@Test
	public void staleFlushedWriterCannotOverwriteACompletedCommit() throws Exception {
		StrolchTransactionException failure = assertThrows(StrolchTransactionException.class, () -> {
			try (StrolchTransaction stale = open(false)) {
				set(stale, "a", "2");
				stale.flush();
				result(this.executor.submit(() -> {
					try (StrolchTransaction winner = open(false)) {
						set(winner, "b", "3");
						winner.commitOnClose();
					}
					return null;
				}));
				stale.commitOnClose();
			}
		});
		assertTrue(failure.getCause() instanceof StrolchTransactionConflictException);
		assertEquals("1", read("a"));
		assertEquals("3", read("b"));
	}

	@Test
	public void bulkRemovalCannotChangeAnExistingSnapshot() throws Exception {
		try (StrolchTransaction reader = open(true)) {
			assertEquals(2, reader.getResourceMap().querySize(reader, this.type));
			result(this.executor.submit(() -> {
				try (StrolchTransaction writer = open(false)) {
					writer.getResourceMap().removeAllBy(writer, this.type);
					writer.commitOnClose();
				}
				return null;
			}));
			assertEquals("1", get(reader, "a").getName());
			assertEquals(2, reader.getResourceMap().querySize(reader, this.type));
		}
		try (StrolchTransaction reader = open(true)) {
			assertEquals(0, reader.getResourceMap().querySize(reader, this.type));
		}
	}

	@Test
	public void deleteAndRecreateKeepsRetainedSnapshot() throws Exception {
		try (StrolchTransaction reader = open(true)) {
			Resource retained = get(reader, "a");
			result(this.executor.submit(() -> {
				try (StrolchTransaction writer = open(false)) {
					writer.remove(writer.readLock(get(writer, "a")));
					writer.flush();
					writer.add(new Resource("a", "3", this.type));
					writer.commitOnClose();
				}
				return null;
			}));
			assertEquals("1", retained.getName());
			assertEquals("1", get(reader, "a").getName());
		}
		assertEquals("3", read("a"));
	}

	@Test
	public void lockExcludesAnotherTransactionAndReleasesOnClose() throws Exception {
		try (StrolchTransaction owner = open(false)) {
			owner.readLock(get(owner, "a"));
			result(this.executor.submit(() -> {
				try (StrolchTransaction contender = open(false)) {
					assertThrows(ElementLockingException.class, () -> contender.readLock(get(contender, "a")));
				}
				return null;
			}));
		}
		result(this.executor.submit(() -> {
			try (StrolchTransaction next = open(false)) {
				next.readLock(get(next, "a"));
			}
			return null;
		}));
	}
}
