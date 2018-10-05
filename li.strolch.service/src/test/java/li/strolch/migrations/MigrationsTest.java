/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.migrations;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.ModelGenerator;
import li.strolch.model.Order;
import li.strolch.persistence.api.AddOrderCommand;
import li.strolch.persistence.api.RemoveOrderCommand;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.StrolchConstants;
import li.strolch.testbase.runtime.RuntimeMock;
import li.strolch.utils.Version;
import li.strolch.utils.collections.MapOfLists;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

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

		certificate = runtimeMock.getPrivilegeHandler().authenticate("test", "test".toCharArray());
	}

	@AfterClass
	public static void afterClass() {
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();
	}

	@Test
	public void shouldRunMigrations() {

		MigrationsHandler migrationsHandler = runtimeMock.getContainer().getComponent(MigrationsHandler.class);
		Map<String, MigrationVersion> currentVersions = migrationsHandler.getCurrentVersions(certificate);
		String defRealm = StrolchConstants.DEFAULT_REALM;
		assertEquals("1.1.1", currentVersions.get(defRealm).getDataVersion().toString());
		assertEquals("0.0.0", currentVersions.get("other").getCodeVersion().toString());

		MapOfLists<String, Version> lastMigrations = migrationsHandler.getLastMigrations();
		List<Version> expectedMigrations = Arrays
				.asList(Version.valueOf("0.1.0"), Version.valueOf("0.1.1"), Version.valueOf("0.5.2"),
						Version.valueOf("1.0.0"), Version.valueOf("1.0.5"), Version.valueOf("1.1.1"));
		assertEquals(expectedMigrations, lastMigrations.getList(defRealm));
		assertEquals(null, lastMigrations.getList("other"));

		MapOfLists<String, Version> migrationsToRun = migrationsHandler.queryMigrationsToRun(certificate);
		assertTrue("Expected to have all migrations run", migrationsToRun.isEmpty());

		// assert new current version
		currentVersions = migrationsHandler.getCurrentVersions(certificate);
		assertEquals("1.1.1", currentVersions.get(defRealm).getDataVersion().toString());
		assertEquals("0.0.0", currentVersions.get("other").getCodeVersion().toString());

		MapOfLists<String, CodeMigration> codeMigrationsByRealm = new MapOfLists<>();
		// add migrations in wrong sequence - should be fixed by migration handler
		codeMigrationsByRealm.addElement(defRealm, new MyMigration2(defRealm));
		codeMigrationsByRealm.addElement(defRealm, new MyMigration1(defRealm));
		codeMigrationsByRealm.addElement(defRealm, new MyMigration0(defRealm));
		migrationsHandler.runCodeMigrations(certificate, codeMigrationsByRealm);

		lastMigrations = migrationsHandler.getLastMigrations();
		assertEquals(1, lastMigrations.keySet().size());
		assertEquals(Arrays.asList(Version.valueOf("1.0.0"), Version.valueOf("1.2.0"), Version.valueOf("1.2.0.a")),
				lastMigrations.getList(defRealm));

		// assert new current version
		currentVersions = migrationsHandler.getCurrentVersions(certificate);
		assertEquals("1.2.0.a", currentVersions.get(defRealm).getCodeVersion().toString());
		assertEquals("0.0.0", currentVersions.get("other").getDataVersion().toString());
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

				buildMigrationVersionChangeCommand(container, tx);

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

				Order fooOrder = ModelGenerator.createOrder("foo1", "Foo", "Foo");

				AddOrderCommand addOrderCommand = new AddOrderCommand(container, tx);
				addOrderCommand.setOrder(fooOrder);
				tx.addCommand(addOrderCommand);

				buildMigrationVersionChangeCommand(container, tx);

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

				buildMigrationVersionChangeCommand(container, tx);

				tx.commitOnClose();
			}
		}
	}
}
