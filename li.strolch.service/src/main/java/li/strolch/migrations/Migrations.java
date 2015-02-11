package li.strolch.migrations;

import java.io.File;
import java.io.FileFilter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.SortedSet;
import java.util.TreeSet;

import li.strolch.agent.api.ComponentContainer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.Version;
import ch.eitchnet.utils.collections.MapOfLists;
import ch.eitchnet.utils.dbc.DBC;

public class Migrations {

	private static final Logger logger = LoggerFactory.getLogger(Migrations.class);

	private ComponentContainer container;
	private Map<String, Version> currentVersions;
	private boolean verbose;

	private Map<String, SortedSet<DataMigration>> dataMigrations;
	private Map<String, SortedSet<CodeMigration>> codeMigrations;

	private MapOfLists<String, Version> migrationsRan;

	public Migrations(ComponentContainer container, Map<String, Version> currentVersions, boolean verbose) {
		this.container = container;
		this.currentVersions = currentVersions;
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
		this.dataMigrations = loadDataMigrations(this.currentVersions, migrationsPath);

		// code migrations
		this.codeMigrations = loadCodeMigrations(this.currentVersions, migrationsPath);

		// log found migrations
		if (this.verbose)
			logDetectedMigrations(this.currentVersions, this.dataMigrations, this.codeMigrations);
	}

	public void runMigrations(Certificate certificate) {

		MapOfLists<String, Version> migrationsRan = new MapOfLists<>();

		for (Entry<String, Version> entry : this.currentVersions.entrySet()) {
			String realm = entry.getKey();
			Version currentVersion = entry.getValue();

			logger.info("[" + realm + "] Performing all migrations after " + currentVersion);

			Version nextPossibleVersion = currentVersion.add(0, 0, 1);
			CodeMigration currentCodeMigration = new CodeMigration(realm, nextPossibleVersion, null);
			DataMigration currentDataMigration = new DataMigration(realm, nextPossibleVersion, null);

			SortedSet<CodeMigration> codeMigrations = this.codeMigrations.get(realm);
			if (codeMigrations != null && !codeMigrations.isEmpty()) {
				for (CodeMigration migration : codeMigrations.tailSet(currentCodeMigration)) {
					migration.migrate(container, certificate);
					migrationsRan.addElement(realm, migration.getVersion());
				}
			}

			SortedSet<DataMigration> dataMigrations = this.dataMigrations.get(realm);
			if (dataMigrations != null && !dataMigrations.isEmpty()) {
				for (DataMigration migration : dataMigrations.tailSet(currentDataMigration)) {
					migration.migrate(container, certificate);
					migrationsRan.addElement(realm, migration.getVersion());
				}
			}
		}

		if (migrationsRan.isEmpty()) {
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
	public void runCodeMigrations(Certificate cert, MapOfLists<String, CodeMigration> codeMigrationsByRealm) {

		MapOfLists<String, Version> migrationsRan = new MapOfLists<>();

		for (String realm : codeMigrationsByRealm.keySet()) {
			Version currentVersion = this.currentVersions.get(realm);

			List<CodeMigration> listOfMigrations = codeMigrationsByRealm.getList(realm);
			SortedSet<CodeMigration> migrations = new TreeSet<>((o1, o2) -> o1.getVersion().compareTo(o2.getVersion()));
			migrations.addAll(listOfMigrations);

			Version nextVersion = currentVersion.add(0, 0, 1);
			CodeMigration nextMigration = new CodeMigration(realm, nextVersion);

			SortedSet<CodeMigration> migrationsToRun = migrations.tailSet(nextMigration);
			for (CodeMigration migration : migrationsToRun) {
				DBC.INTERIM.assertEquals("Realms do not match!", realm, migration.getRealm());
				Version migrateVersion = migration.getVersion();
				boolean isLaterMigration = migrateVersion.compareTo(currentVersion) > 0;
				DBC.INTERIM.assertTrue("Current version " + currentVersion + " is not before next " + migrateVersion,
						isLaterMigration);

				migration.migrate(this.container, cert);
				migrationsRan.addElement(realm, migration.getVersion());
			}
		}

		if (migrationsRan.isEmpty()) {
			logger.info("There were no migrations required!");
		} else {
			logger.info("Migrated " + migrationsRan.size() + " realms!");
		}
		this.migrationsRan = migrationsRan;
	}

	private static void logDetectedMigrations(Map<String, Version> currentVersions,
			Map<String, SortedSet<DataMigration>> allDataMigrations,
			Map<String, SortedSet<CodeMigration>> allCodeMigrations) {
		for (Entry<String, Version> entry : currentVersions.entrySet()) {
			String realm = entry.getKey();
			Version currentVersion = entry.getValue();

			SortedSet<CodeMigration> codeMigrations = allCodeMigrations.get(realm);
			if (codeMigrations == null || codeMigrations.isEmpty()) {
				logger.info("[" + realm + "] Found no code migrations.");
			} else {
				logger.info("[" + realm + "] Found " + codeMigrations.size() + " code migrations");
				for (CodeMigration codeMigration : codeMigrations) {
					if (codeMigration.getVersion().compareTo(currentVersion) > 0)
						logger.info("[" + realm + "] + " + codeMigration.getVersion().toString());
					else
						logger.info("[" + realm + "] - " + codeMigration.getVersion().toString());
				}
			}

			SortedSet<DataMigration> dataMigrations = allDataMigrations.get(realm);
			if (dataMigrations == null || dataMigrations.isEmpty()) {
				logger.info("[" + realm + "] Found no data migrations.");
			} else {
				logger.info("[" + realm + "] Found " + dataMigrations.size() + " data migrations");
				for (DataMigration dataMigration : dataMigrations) {
					if (dataMigration.getVersion().compareTo(currentVersion) > 0)
						logger.info("[" + realm + "] + " + dataMigration.getVersion().toString());
					else
						logger.info("[" + realm + "] - " + dataMigration.getVersion().toString());
				}
			}
		}
	}

	private static Map<String, SortedSet<DataMigration>> loadDataMigrations(Map<String, Version> currentVersions,
			File migrationsPath) {

		Map<String, SortedSet<DataMigration>> migrationsByRealm = new HashMap<>();

		File dataDir = new File(migrationsPath, "data");
		if (dataDir.exists()) {
			DBC.PRE.assertTrue("migrations/data must be a directory!", dataDir.isDirectory());

			// only list directories where name is a realmName
			File[] realmMigrations = dataDir
					.listFiles((FileFilter) path -> currentVersions.containsKey(path.getName()));

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

	private static Map<String, SortedSet<CodeMigration>> loadCodeMigrations(Map<String, Version> currentVersions,
			File migrationsPath) {

		Map<String, SortedSet<CodeMigration>> migrationsByRealm = new HashMap<>(); //new TreeSet<>((o1, o2) -> o1.getVersion().compareTo(o2.getVersion()));

		File codeDir = new File(migrationsPath, "code");
		if (codeDir.exists()) {
			DBC.PRE.assertTrue("migrations/code must be a directory!", codeDir.isDirectory());

			File[] realmMigrations = codeDir
					.listFiles((FileFilter) path -> currentVersions.containsKey(path.getName()));
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

	public MapOfLists<String, Version> getMigrationsToRun() {

		MapOfLists<String, Version> migrationsToRun = new MapOfLists<>();

		for (Entry<String, Version> entry : this.currentVersions.entrySet()) {
			String realm = entry.getKey();
			Version nextPossibleVersion = entry.getValue().add(0, 0, 1);
			CodeMigration currentCodeMigration = new CodeMigration(realm, nextPossibleVersion, null);
			DataMigration currentDataMigration = new DataMigration(realm, nextPossibleVersion, null);

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
