package li.strolch.migrations;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.command.AddOrderCommand;
import li.strolch.command.RemoveOrderCommand;
import li.strolch.model.ModelGenerator;
import li.strolch.model.Order;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;
import li.strolch.testbase.runtime.RuntimeMock;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.Version;
import ch.eitchnet.utils.collections.MapOfLists;

public class MigrationsTest {

	public static final String RUNTIME_PATH = "target/migrationstest/"; //$NON-NLS-1$
	public static final String CONFIG_SRC = "src/test/resources/migrationstest"; //$NON-NLS-1$

	protected static RuntimeMock runtimeMock;
	protected static Certificate certificate;

	@BeforeClass
	public static void beforeClass() throws Exception {

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		runtimeMock.startContainer();

		certificate = runtimeMock.getPrivilegeHandler().authenticate("test", "test".getBytes());
	}

	@AfterClass
	public static void afterClass() {
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();
	}

	@Test
	public void shouldRunMigrations() {

		MigrationsHandler migrationsHandler = runtimeMock.getContainer().getComponent(MigrationsHandler.class);
		Map<String, Version> currentVersions = migrationsHandler.getCurrentVersions(certificate);
		String defRealm = StrolchConstants.DEFAULT_REALM;
		assertEquals("1.1.1", currentVersions.get(defRealm).toString());
		assertEquals("0.0.0", currentVersions.get("other").toString());

		MapOfLists<String, Version> lastMigrations = migrationsHandler.getLastMigrations();
		List<Version> expectedMigrations = Arrays.asList(Version.valueOf("0.1.0"), Version.valueOf("0.1.1"),
				Version.valueOf("0.5.2"), Version.valueOf("1.0.0"), Version.valueOf("1.0.5"), Version.valueOf("1.1.1"));
		assertEquals(expectedMigrations, lastMigrations.getList(defRealm));
		assertEquals(null, lastMigrations.getList("other"));

		MapOfLists<String, Version> migrationsToRun = migrationsHandler.queryMigrationsToRun(certificate);
		assertTrue("Expected to have all migrations run", migrationsToRun.isEmpty());

		// assert new current version
		currentVersions = migrationsHandler.getCurrentVersions(certificate);
		assertEquals("1.1.1", currentVersions.get(defRealm).toString());
		assertEquals("0.0.0", currentVersions.get("other").toString());

		MapOfLists<String, CodeMigration> codeMigrationsByRealm = new MapOfLists<>();
		// add migrations in wrong sequence - should be fixed by migration handler
		codeMigrationsByRealm.addElement(defRealm, new MyMigration2(defRealm));
		codeMigrationsByRealm.addElement(defRealm, new MyMigration1(defRealm));
		codeMigrationsByRealm.addElement(defRealm, new MyMigration0(defRealm));
		migrationsHandler.runCodeMigrations(certificate, codeMigrationsByRealm);

		lastMigrations = migrationsHandler.getLastMigrations();
		assertEquals(1, lastMigrations.keySet().size());
		assertEquals(Arrays.asList(Version.valueOf("1.2.0"), Version.valueOf("1.2.0.a")),
				lastMigrations.getList(defRealm));

		// assert new current version
		currentVersions = migrationsHandler.getCurrentVersions(certificate);
		assertEquals("1.2.0.a", currentVersions.get(defRealm).toString());
		assertEquals("0.0.0", currentVersions.get("other").toString());
	}

	private static class MyMigration0 extends CodeMigration {

		private static final Version version = Version.valueOf("1.0.0");

		public MyMigration0(String realm) {
			super(realm, version);
		}

		@Override
		public void migrate(ComponentContainer container, Certificate cert) {
			logger.info("[" + this.realm + "] Running migration " + this.getVersion());

			try (StrolchTransaction tx = openTx(container, cert)) {

				Order fooOrder = ModelGenerator.createOrder("foo", "Foo", "Foo");

				AddOrderCommand addOrderCommand = new AddOrderCommand(container, tx);
				addOrderCommand.setOrder(fooOrder);
				tx.addCommand(addOrderCommand);

				tx.addCommand(buildMigrationVersionChangeCommand(container, tx));

				tx.commitOnClose();
			}
		}
	}

	private static class MyMigration1 extends CodeMigration {

		private static final Version version = Version.valueOf("1.2.0");

		public MyMigration1(String realm) {
			super(realm, version);
		}

		@Override
		public void migrate(ComponentContainer container, Certificate cert) {
			logger.info("[" + this.realm + "] Running migration " + this.getVersion());

			try (StrolchTransaction tx = openTx(container, cert)) {

				Order fooOrder = ModelGenerator.createOrder("foo", "Foo", "Foo");

				AddOrderCommand addOrderCommand = new AddOrderCommand(container, tx);
				addOrderCommand.setOrder(fooOrder);
				tx.addCommand(addOrderCommand);

				tx.addCommand(buildMigrationVersionChangeCommand(container, tx));

				tx.commitOnClose();
			}
		}
	}

	private static class MyMigration2 extends CodeMigration {

		private static final Version version = Version.valueOf("1.2.0.a");

		public MyMigration2(String realm) {
			super(realm, version);
		}

		@Override
		public void migrate(ComponentContainer container, Certificate cert) {
			logger.info("[" + this.realm + "] Running migration " + this.getVersion());

			try (StrolchTransaction tx = openTx(container, cert)) {

				Order fooOrder = tx.getOrderBy("Foo", "foo");

				RemoveOrderCommand removeOrderCommand = new RemoveOrderCommand(container, tx);
				removeOrderCommand.setOrder(fooOrder);
				tx.addCommand(removeOrderCommand);

				tx.addCommand(buildMigrationVersionChangeCommand(container, tx));

				tx.commitOnClose();
			}
		}
	}
}
