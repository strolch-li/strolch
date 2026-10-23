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

import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.search.ResourceSearch;
import li.strolch.testbase.runtime.RuntimeMock;
import li.strolch.utils.concurrent.ElementLockingException;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.junit.Assert.*;

/**
 * Test-first litmus cases, not a complete ACID certification. Unexpected aborts/timeouts fail diagnostically;
 * they must not be classified as isolation violations without inspecting the history. See docs/ACID-BASELINE.md.
 */
@RunWith(Parameterized.class)
public class TransactionIsolationBaselineTest {

	@BeforeClass
	public static void requireExplicitDiagnosticRun() {
		org.junit.Assume.assumeTrue("Enable the diagnostic suite with -Dacid.baseline=true",
				Boolean.getBoolean("acid.baseline"));
	}

	@Parameterized.Parameters(name = "{0}")
	public static List<Object[]> modes() {
		// Explicit transient-only execution is useful without PostgreSQL; it does not test durability.
		if (Boolean.getBoolean("acid.transientOnly"))
			return List.<Object[]>of(new Object[]{"transient", false, false});
		return List.of(new Object[]{"transient", false, false}, new Object[]{"postgres", true, false},
				new Object[]{"postgres-history", true, true});
	}

	private final String mode;
	private final boolean persistent;
	private final boolean history;
	private final String type = "AcidBaseline-" + UUID.randomUUID();
	private RuntimeMock runtime;
	private Certificate certificate;
	private ExecutorService executor;

	public TransactionIsolationBaselineTest(String mode, boolean persistent, boolean history) {
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
		result(first);
		result(second);
		assertFalse("Both committed after reading the other as enabled; no serial execution permits this",
				read("a").equals("0") && read("b").equals("0"));
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
