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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Comparator;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import li.strolch.agent.impl.InternalStrolchRealm;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.StrolchCommitOutcomeUnknownException;
import li.strolch.persistence.api.StrolchPostCommitException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.api.StrolchTransactionException;
import li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler;
import li.strolch.persistence.postgresql.PostgreSqlStrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.search.ResourceSearch;
import li.strolch.service.api.Command;
import li.strolch.testbase.runtime.RuntimeMock;
import li.strolch.utils.concurrent.ElementLockingException;
import org.junit.After;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import static li.strolch.model.activity.TimeOrdering.SERIES;
import static li.strolch.persistence.api.TransactionState.COMMITTED;
import static li.strolch.persistence.api.TransactionState.FAILED;
import static li.strolch.persistence.api.TransactionState.UNKNOWN;
import static org.junit.Assert.*;

/**
 * Transaction-local flush and commit publication regressions. PostgreSQL modes require -Dtransaction.postgres=true
 * and a disposable database on transaction.dbPort (55432 by default). Ordinary reads are not transaction snapshots.
 */
@RunWith(Parameterized.class)
public class TransactionPublicationTest {

	@Parameterized.Parameters(name = "{0}")
	public static List<Object[]> modes() {
		// Explicit transient-only execution is useful without PostgreSQL; it does not test durability.
		if (!Boolean.getBoolean("transaction.postgres"))
			return List.<Object[]>of(new Object[]{"transient", false, false});
		return List.of(new Object[]{"transient", false, false}, new Object[]{"postgres", true, false},
				new Object[]{"postgres-history", true, true});
	}

	private final String mode;
	private final boolean persistent;
	private final boolean history;
	private final String type = "TransactionPublication-" + UUID.randomUUID();
	private RuntimeMock runtime;
	private Certificate certificate;
	private ExecutorService executor;

	public TransactionPublicationTest(String mode, boolean persistent, boolean history) {
		this.mode = mode;
		this.persistent = persistent;
		this.history = history;
	}

	@Before
	public void setUp() throws Exception {
		Path root = Path.of("target", getClass().getSimpleName(), this.mode, this.type);
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
		if (this.persistent)
			configurePostgres(source.resolve("config/StrolchConfiguration.xml"));
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

	private void configurePostgres(Path config) throws Exception {
		String xml = Files.readString(config)
				.replace("<depends>PrivilegeHandler</depends>", "<depends>PrivilegeHandler</depends><depends>PersistenceHandler</depends>")
				.replace("<dataStoreMode>TRANSIENT</dataStoreMode>", "<dataStoreMode>CACHED</dataStoreMode><enableVersioning>"
						+ this.history + "</enableVersioning>");
		// Dedicated disposable DB; never point this fixture at customer data. No schema-drop operation is performed.
		String persistence = """
				<Component>
					<name>PersistenceHandler</name>
					<api>li.strolch.persistence.api.PersistenceHandler</api>
					<impl>li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler</impl>
					<Properties>
						<allowSchemaCreation>true</allowSchemaCreation>
						<allowSchemaMigration>false</allowSchemaMigration>
						<allowSchemaDrop>false</allowSchemaDrop>
						<db.allowHostOverrideEnv>true</db.allowHostOverrideEnv>
						<db.url>jdbc:postgresql://localhost/strolch_acid_test</db.url>
						<db.username>strolch_acid_test</db.username>
						<db.password>test</db.password>
						<db.pool.maximumPoolSize>4</db.pool.maximumPoolSize>
					</Properties>
				</Component>
				""";
		int port = Integer.getInteger("transaction.dbPort", 55432);
		if (port < 1 || port > 65535)
			throw new IllegalArgumentException("Invalid transaction.dbPort: " + port);
		persistence = persistence.replace("localhost/strolch_acid_test", "localhost:" + port + "/strolch_acid_test");
		Files.writeString(config, xml.replace("</env>", persistence + "</env>"));
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
						// This command deliberately injects a failure; it has no preconditions.
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
	public void independentWritersBothCommit() throws Exception {
		CountDownLatch bothFlushed = new CountDownLatch(2);
		Future<?> first = this.executor.submit(() -> updateIndependent("a", bothFlushed));
		Future<?> second = this.executor.submit(() -> updateIndependent("b", bothFlushed));
		result(first);
		result(second);
		assertEquals("2", read("a"));
		assertEquals("2", read("b"));
	}

	private Void updateIndependent(String id, CountDownLatch bothFlushed) throws InterruptedException {
		try (StrolchTransaction tx = open(false)) {
			set(tx, id, "2");
			tx.flush();
			bothFlushed.countDown();
			await(bothFlushed);
			tx.commitOnClose();
		}
		return null;
	}

	@Test
	public void flushIncludesOwnInsertUpdateAndDelete() throws Exception {
		try (StrolchTransaction tx = open(false)) {
			set(tx, "a", "2");
			tx.remove(get(tx, "b"));
			tx.add(new Resource("c", "3", this.type));
			tx.flush();
			assertEquals("2", get(tx, "a").getName());
			assertFalse(tx.hasResource(this.type, "b"));
			assertNull(tx.getResourceBy(this.type, "b", false));
			assertEquals(2, tx.getResourceMap().querySize(tx, this.type));
			assertEquals(List.of("a", "c"), new ResourceSearch().types(this.type).search(tx).toList().stream()
					.map(Resource::getId).sorted().toList());
			tx.flush();
			assertEquals("1", result(this.executor.submit(() -> read("a"))));
			tx.commitOnClose();
		}
		assertEquals("2", read("a"));
		try (StrolchTransaction tx = open(true)) {
			assertFalse(tx.hasResource(this.type, "b"));
			assertEquals("3", get(tx, "c").getName());
		}
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
	@Test
	public void flushExecutesCommandsOnlyOnce() {
		AtomicInteger executions = new AtomicInteger();
		try (StrolchTransaction tx = open(false)) {
			tx.addCommand(new Command(tx) {
				@Override public void validate() {
					// Counting executions needs no command preconditions.
				}
				@Override public void doCommand() { executions.incrementAndGet(); set(tx, "a", "2"); }
			});
			tx.flush();
			tx.flush();
			assertEquals(1, executions.get());
			tx.commitOnClose();
		}
		assertEquals(1, executions.get());
		assertEquals("2", read("a"));
	}

	@Test
	public void allRootKindsRemainPrivateUntilCommit() throws Exception {
		try (StrolchTransaction tx = open(false)) {
			tx.add(new Order("order", "local", this.type));
			tx.add(new Activity("activity", "local", this.type, SERIES));
			tx.flush();
			assertNotNull(tx.getOrderBy(this.type, "order", true));
			assertNotNull(tx.getActivityBy(this.type, "activity", true));
			result(this.executor.submit(() -> {
				try (StrolchTransaction other = open(true)) {
					assertNull(other.getOrderBy(this.type, "order", false));
					assertNull(other.getActivityBy(this.type, "activity", false));
				}
				return null;
			}));
			tx.commitOnClose();
		}
		try (StrolchTransaction tx = open(true)) {
			assertNotNull(tx.getOrderBy(this.type, "order", true));
			assertNotNull(tx.getActivityBy(this.type, "activity", true));
		}
	}

	@Test
	public void typeClearIsLocalAndCanBeRepopulated() throws Exception {
		try (StrolchTransaction tx = open(false)) {
			assertEquals(2, tx.getResourceMap().removeAllBy(tx, this.type));
			tx.add(new Resource("a", "replacement", this.type));
			tx.flush();
			assertEquals(1, tx.getResourceMap().querySize(tx, this.type));
			assertEquals("1", result(this.executor.submit(() -> read("a"))));
			tx.commitOnClose();
		}
		assertEquals("replacement", read("a"));
	}

	@Test
	public void repeatedFlushesRetainVersionHistory() {
		Assume.assumeTrue(this.history);
		try (StrolchTransaction tx = open(false)) {
			set(tx, "a", "2");
			tx.flush();
			set(tx, "a", "3");
			tx.flush();
			tx.commitOnClose();
		}
		try (StrolchTransaction tx = open(true)) {
			assertEquals("3", get(tx, "a").getName());
			assertEquals(List.of("1", "2", "3"), tx.getResourceMap().getVersionsFor(tx, this.type, "a").stream()
					.sorted(Comparator.comparingInt(e -> e.getVersion().getVersion())).map(Resource::getName).toList());
		}
	}

	private PostgreSqlPersistenceHandler postgres() {
		return (PostgreSqlPersistenceHandler) this.runtime.getContainer()
				.getComponent(PersistenceHandler.class);
	}

	private PostgreSqlStrolchTransaction instrumented(
			Connection connection) {
		return new PostgreSqlStrolchTransaction(this.runtime.getContainer(),
				this.runtime.getContainer().getRealm(this.certificate), this.certificate, getClass().getName(), false,
				postgres(), connection);
	}

	@FunctionalInterface
	private interface ConnectionCall { Object invoke(Method method, Object[] args) throws Throwable; }

	private Connection proxy(ConnectionCall call) {
		return (Connection) Proxy.newProxyInstance(getClass().getClassLoader(),
				new Class<?>[]{Connection.class}, (_, method, args) -> call.invoke(method, args));
	}

	private Object invoke(Connection connection, Method method, Object[] args) throws Throwable {
		try { return method.invoke(connection, args); }
		catch (InvocationTargetException e) { throw e.getCause(); }
	}

	private String committedName(Connection connection) throws SQLException {
		try (var statement = connection.prepareStatement("select name from resources where type=? and id='a' and latest=true")) {
			statement.setString(1, this.type);
			try (var rows = statement.executeQuery()) { assertTrue(rows.next()); return rows.getString(1); }
		}
	}

	@Test
	public void sqlCommitPrecedesMemoryPublicationAndFlushDoesNotTouchSql() throws Exception {
		Assume.assumeTrue(this.persistent);
		AtomicInteger sqlCalls = new AtomicInteger();
		Connection connection = postgres().getConnection("defaultRealm");
		Connection tracked = proxy((method, args) -> {
			if (method.getName().equals("prepareStatement")) sqlCalls.incrementAndGet();
			if (method.getName().equals("commit")) {
				assertEquals("1", ((InternalStrolchRealm) this.runtime.getContainer().getRealm(this.certificate)).getResourceMap()
						.stream(null, this.type).filter(e -> e.getId().equals("a")).findFirst().orElseThrow().getName());
				Object returned = invoke(connection, method, args);
				assertEquals("2", committedName(connection));
				assertEquals("1", ((InternalStrolchRealm) this.runtime.getContainer().getRealm(this.certificate)).getResourceMap()
						.stream(null, this.type).filter(e -> e.getId().equals("a")).findFirst().orElseThrow().getName());
				return returned;
			}
			return invoke(connection, method, args);
		});
		try (StrolchTransaction tx = instrumented(tracked)) {
			set(tx, "a", "2");
			tx.add(new Resource("c", "local", this.type));
			tx.flush();
			assertEquals("Local flush must not even prepare SQL", 0, sqlCalls.get());
			tx.commitOnClose();
		}
		assertTrue(sqlCalls.get() > 0);
		assertEquals("2", read("a"));
	}

	@Test
	public void sqlWriteFailureDoesNotPublish() throws Exception {
		Assume.assumeTrue(this.persistent);
		Connection connection = postgres().getConnection("defaultRealm");
		var tx = instrumented(proxy((method, args) -> {
			if (method.getName().equals("prepareStatement")) throw new SQLException("Injected SQL failure");
			return invoke(connection, method, args);
		}));
		assertThrows(StrolchTransactionException.class, () -> {
			try (tx) { set(tx, "a", "2"); tx.flush(); tx.commitOnClose(); }
		});
		assertEquals(FAILED, tx.getState());
		assertEquals("1", read("a"));
		try (var verification = postgres().getConnection("defaultRealm")) { assertEquals("1", committedName(verification)); }
	}

	@Test
	public void lostCommitResponseFencesRealmWithoutUndoingDurableData() throws Exception {
		Assume.assumeTrue(this.persistent);
		Connection connection = postgres().getConnection("defaultRealm");
		var tx = instrumented(proxy((method, args) -> {
			Object returned = invoke(connection, method, args);
			if (method.getName().equals("commit")) throw new SQLException("Injected lost commit response");
			return returned;
		}));
		assertThrows(StrolchCommitOutcomeUnknownException.class, () -> {
			try (tx) { set(tx, "a", "2"); tx.commitOnClose(); }
		});
		assertEquals(UNKNOWN, tx.getState());
		assertThrows(IllegalStateException.class, () -> read("a"));
		try (var verification = postgres().getConnection("defaultRealm")) { assertEquals("2", committedName(verification)); }
	}

	@Test
	public void connectionCleanupFailureAfterCommitStillPublishes() throws Exception {
		Assume.assumeTrue(this.persistent);
		Connection connection = postgres().getConnection("defaultRealm");
		try (var tx = instrumented(proxy((method, args) -> {
			Object returned = invoke(connection, method, args);
			if (method.getName().equals("close")) throw new SQLException("Injected connection cleanup failure");
			return returned;
		}))) {
			set(tx, "a", "2");
			tx.commitOnClose();
		}
		assertEquals("2", read("a"));
	}

	@Test
	public void publicationFailureAfterCommitNeverRollsBackDatabase() throws Exception {
		Assume.assumeTrue(this.persistent);
		Connection connection = postgres().getConnection("defaultRealm");
		AtomicInteger rollbacks = new AtomicInteger();
		var realm = (InternalStrolchRealm) this.runtime.getContainer().getRealm(this.certificate);
		var tx = instrumented(proxy((method, args) -> {
			if (method.getName().equals("rollback")) rollbacks.incrementAndGet();
			Object returned = invoke(connection, method, args);
			if (method.getName().equals("commit")) realm.getModelPublication().fail(new IllegalStateException("Injected publication failure"));
			return returned;
		}));
		assertThrows(StrolchPostCommitException.class, () -> {
			try (tx) { set(tx, "a", "2"); tx.commitOnClose(); }
		});
		assertEquals(COMMITTED, tx.getState());
		assertEquals(0, rollbacks.get());
		assertThrows(IllegalStateException.class, () -> read("a"));
		try (var verification = postgres().getConnection("defaultRealm")) { assertEquals("2", committedName(verification)); }
	}

	@Test
	public void resetDiscardsFlushedModelChanges() {
		try (StrolchTransaction tx = open(false)) {
			set(tx, "a", "2");
			tx.flush();
			tx.reset();
			assertEquals("1", get(tx, "a").getName());
			set(tx, "b", "3");
			tx.commitOnClose();
		}
		assertEquals("1", read("a"));
		assertEquals("3", read("b"));
	}

	@Test
	public void versionUndoAndRevertStayLocal() {
		Assume.assumeTrue(this.history);
		try (StrolchTransaction tx = open(false)) {
			set(tx, "a", "2");
			tx.flush();
			Resource previous = tx.getResourceMap().undoVersion(tx, get(tx, "a"));
			assertEquals("1", previous.getName());
			assertEquals(1, tx.getResourceMap().getVersionsFor(tx, this.type, "a").size());
			tx.clearCache();
			assertEquals("1", get(tx, "a").getName());
			tx.getResourceMap().revertToVersion(tx, this.type, "a", 0);
			tx.commitOnClose();
		}
		assertEquals("1", read("a"));
	}

	@Test
	public void updatingHistoryUsesLockedVersionWithoutAnotherSqlLookup() throws Exception {
		Assume.assumeTrue(this.history);
		Connection connection = postgres().getConnection("defaultRealm");
		try (var tx = instrumented(proxy((method, args) -> {
			if (method.getName().equals("prepareStatement") && ((String) args[0]).contains("max(version)"))
				throw new AssertionError("An update already knows the locked committed version");
			return invoke(connection, method, args);
		}))) {
			set(tx, "a", "2");
			tx.flush();
			tx.commitOnClose();
		}
		assertEquals("2", read("a"));
	}

}
