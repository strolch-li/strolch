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

import java.io.File;
import java.io.FileFilter;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.privilege.model.Certificate;
import li.strolch.utils.Version;
import li.strolch.utils.collections.MapOfLists;
import li.strolch.utils.dbc.DBC;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Migrations {

	private static final Logger logger = LoggerFactory.getLogger(Migrations.class);

	private ComponentContainer container;
	private Set<String> realmNames;
	private boolean verbose;

	private Map<String, SortedSet<DataMigration>> dataMigrations;
	private Map<String, SortedSet<CodeMigration>> codeMigrations;

	private MapOfLists<String, Version> migrationsRan;

	public Migrations(ComponentContainer container, Set<String> realmNames, boolean verbose) {
		this.container = container;
		this.realmNames = realmNames;
		this.verbose = verbose;
	}

	public void setVerbose(boolean verbose) {
		this.verbose = verbose;
	}

	public boolean isVerbose() {
		return verbose;
	}

	public MapOfLists<String, Version> getMigrationsRan() {
		return this.migrationsRan;
	}

	public void parseMigrations(File migrationsPath) {
		DBC.PRE.assertTrue("If migrations path is not a directory!", migrationsPath.isDirectory());

		// data migrations
		this.dataMigrations = loadDataMigrations(this.realmNames, migrationsPath);

		// code migrations
		this.codeMigrations = loadCodeMigrations(this.realmNames, migrationsPath);

		// log found migrations
		if (this.verbose)
			logDetectedMigrations(this.realmNames, this.dataMigrations, this.codeMigrations);
	}

	public void runMigrations(Certificate certificate, Map<String, MigrationVersion> currentVersions) {

		MapOfLists<String, Version> migrationsRan = new MapOfLists<>();

		for (Entry<String, MigrationVersion> entry : currentVersions.entrySet()) {
			String realm = entry.getKey();
			MigrationVersion currentVersion = entry.getValue();

			if (this.verbose)
				logger.info("[" + realm + "] Performing all migrations after " + currentVersion);

			Version nextPossibleCodeVersion = currentVersion.getCodeVersion().add(0, 0, 1);
			Version nextPossibleDataVersion = currentVersion.getDataVersion().add(0, 0, 1);
			CodeMigration currentCodeMigration = new CodeMigration(realm, nextPossibleCodeVersion, null);
			DataMigration currentDataMigration = new DataMigration(realm, nextPossibleDataVersion, null);

			SortedSet<DataMigration> dataMigrations = this.dataMigrations.get(realm);
			if (dataMigrations != null && !dataMigrations.isEmpty()) {
				for (DataMigration migration : dataMigrations.tailSet(currentDataMigration)) {
					String msg = "[{0}] Running data migration {1}";
					logger.info(MessageFormat.format(msg, realm, migration.getVersion()));
					migration.migrate(container, certificate);
					migrationsRan.addElement(realm, migration.getVersion());
				}
			}

			SortedSet<CodeMigration> codeMigrations = this.codeMigrations.get(realm);
			if (codeMigrations != null && !codeMigrations.isEmpty()) {
				for (CodeMigration migration : codeMigrations.tailSet(currentCodeMigration)) {
					String msg = "[{0}] Running code migration {1} {2}";
					logger.info(MessageFormat.format(msg, realm, migration.getVersion(), migration.getClass().getName()));
					migration.migrate(container, certificate);
					migrationsRan.addElement(realm, migration.getVersion());
				}
			}
		}

		if (migrationsRan.isEmpty()) {
			if (this.verbose)
				logger.info("There were no migrations required!");
		} else {
			logger.info("Migrated " + migrationsRan.size() + " realms!");
		}

		this.migrationsRan = migrationsRan;
	}

	/**
	 * @param cert
	 * @param codeMigrationsByRealm
	 */
	public void runCodeMigrations(Certificate cert, Map<String, MigrationVersion> currentVersions,
			MapOfLists<String, CodeMigration> codeMigrationsByRealm) {

		MapOfLists<String, Version> migrationsRan = new MapOfLists<>();

		for (String realm : codeMigrationsByRealm.keySet()) {

			// ignore if no such realm
			if (!this.realmNames.contains(realm))
				continue;

			MigrationVersion currentVersion = currentVersions.get(realm);

			List<CodeMigration> listOfMigrations = codeMigrationsByRealm.getList(realm);
			SortedSet<CodeMigration> migrations = new TreeSet<>((o1, o2) -> o1.getVersion().compareTo(o2.getVersion()));
			migrations.addAll(listOfMigrations);

			Version nextVersion = currentVersion.getCodeVersion().add(0, 0, 1);
			CodeMigration nextMigration = new CodeMigration(realm, nextVersion);

			SortedSet<CodeMigration> migrationsToRun = migrations.tailSet(nextMigration);
			for (CodeMigration migration : migrationsToRun) {
				DBC.INTERIM.assertEquals("Realms do not match!", realm, migration.getRealm());
				Version migrateVersion = migration.getVersion();
				boolean isLaterMigration = migrateVersion.compareTo(currentVersion.getCodeVersion()) > 0;
				DBC.INTERIM.assertTrue("Current version " + currentVersion.getCodeVersion() + " is not before next " + migrateVersion,
						isLaterMigration);

				String msg = "[{0}] Running code migration {1} {2}";
				logger.info(MessageFormat.format(msg, realm, migrateVersion, migration.getClass().getName()));
				migration.migrate(this.container, cert);
				migrationsRan.addElement(realm, migration.getVersion());
			}
		}

		if (migrationsRan.isEmpty()) {
			if (this.verbose)
				logger.info("There were no migrations required!");
		} else {
			logger.info("Migrated " + migrationsRan.size() + " realms!");
		}
		this.migrationsRan = migrationsRan;
	}

	private static void logDetectedMigrations(Set<String> realmNames,
			Map<String, SortedSet<DataMigration>> allDataMigrations,
			Map<String, SortedSet<CodeMigration>> allCodeMigrations) {

		for (String realm : realmNames) {

			SortedSet<CodeMigration> codeMigrations = allCodeMigrations.get(realm);
			if (codeMigrations == null || codeMigrations.isEmpty()) {
				logger.info("[" + realm + "] Found no code migrations.");
			} else {
				logger.info("[" + realm + "] Found " + codeMigrations.size() + " code migrations");
				for (CodeMigration codeMigration : codeMigrations) {
					logger.info("[" + realm + "] " + codeMigration.getVersion().toString());
				}
			}

			SortedSet<DataMigration> dataMigrations = allDataMigrations.get(realm);
			if (dataMigrations == null || dataMigrations.isEmpty()) {
				logger.info("[" + realm + "] Found no data migrations.");
			} else {
				logger.info("[" + realm + "] Found " + dataMigrations.size() + " data migrations");
				for (DataMigration dataMigration : dataMigrations) {
					logger.info("[" + realm + "] " + dataMigration.getVersion().toString());
				}
			}
		}
	}

	private static Map<String, SortedSet<DataMigration>> loadDataMigrations(Set<String> realmNames, File migrationsPath) {

		Map<String, SortedSet<DataMigration>> migrationsByRealm = new HashMap<>();

		File dataDir = new File(migrationsPath, "data");
		if (dataDir.exists()) {
			DBC.PRE.assertTrue("migrations/data must be a directory!", dataDir.isDirectory());

			// only list directories where name is a realmName
			File[] realmMigrations = dataDir.listFiles((FileFilter) path -> realmNames.contains(path.getName()));

			for (File realmMigration : realmMigrations) {
				String realm = realmMigration.getName();

				SortedSet<DataMigration> migrations = new TreeSet<>((o1, o2) -> o1.getVersion().compareTo(
						o2.getVersion()));
				migrationsByRealm.put(realm, migrations);

				File[] migrationFiles = realmMigration.listFiles((FileFilter) pathname -> pathname.getName().endsWith(
						".xml"));
				for (File file : migrationFiles) {
					String name = file.getName();
					Version version = Version.valueOf(name.substring(0, name.length() - 4));
					migrations.add(new DataMigration(realm, version, file));
				}
			}
		}

		return migrationsByRealm;
	}

	private static Map<String, SortedSet<CodeMigration>> loadCodeMigrations(Set<String> realmNames, File migrationsPath) {

		Map<String, SortedSet<CodeMigration>> migrationsByRealm = new HashMap<>(); //new TreeSet<>((o1, o2) -> o1.getVersion().compareTo(o2.getVersion()));

		File codeDir = new File(migrationsPath, "code");
		if (codeDir.exists()) {
			DBC.PRE.assertTrue("migrations/code must be a directory!", codeDir.isDirectory());

			File[] realmMigrations = codeDir.listFiles((FileFilter) path -> realmNames.contains(path.getName()));

			for (File realmMigration : realmMigrations) {
				String realm = realmMigration.getName();

				SortedSet<CodeMigration> migrations = new TreeSet<>((o1, o2) -> o1.getVersion().compareTo(
						o2.getVersion()));
				migrationsByRealm.put(realm, migrations);

				File[] migrationFiles = realmMigration.listFiles((FileFilter) pathname -> pathname.getName().endsWith(
						".xml"));
				for (File file : migrationFiles) {
					String name = file.getName();
					Version version = Version.valueOf(name.substring(0, name.length() - 4));
					migrations.add(new CodeMigration(realm, version, file));
				}
			}
		}

		return migrationsByRealm;
	}

	public MapOfLists<String, Version> getMigrationsToRun(Map<String, MigrationVersion> currentVersions) {

		MapOfLists<String, Version> migrationsToRun = new MapOfLists<>();

		for (Entry<String, MigrationVersion> entry : currentVersions.entrySet()) {
			String realm = entry.getKey();
			Version nextPossibleCodeVersion = entry.getValue().getCodeVersion().add(0, 0, 1);
			Version nextPossibleDataVersion = entry.getValue().getDataVersion().add(0, 0, 1);
			CodeMigration currentCodeMigration = new CodeMigration(realm, nextPossibleCodeVersion, null);
			DataMigration currentDataMigration = new DataMigration(realm, nextPossibleDataVersion, null);

			SortedSet<CodeMigration> allCodeMigrations = this.codeMigrations.get(realm);
			if (allCodeMigrations != null) {
				SortedSet<CodeMigration> codeMigrations = allCodeMigrations.tailSet(currentCodeMigration);
				for (CodeMigration codeMigration : codeMigrations) {
					if (!migrationsToRun.containsElement(realm, codeMigration.getVersion()))
						migrationsToRun.addElement(realm, codeMigration.getVersion());
				}
			}

			SortedSet<DataMigration> allDataMigrations = this.dataMigrations.get(realm);
			if (allDataMigrations != null) {
				SortedSet<DataMigration> dataMigrations = allDataMigrations.tailSet(currentDataMigration);
				for (DataMigration dataMigration : dataMigrations) {
					if (!migrationsToRun.containsElement(realm, dataMigration.getVersion()))
						migrationsToRun.addElement(realm, dataMigration.getVersion());
				}
			}
		}

		return migrationsToRun;
	}
}
