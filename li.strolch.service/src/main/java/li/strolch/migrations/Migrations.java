package li.strolch.migrations;

import java.io.File;
import java.io.FileFilter;
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

	private SortedSet<DataMigration> dataMigrations;
	private SortedSet<CodeMigration> codeMigrations;

	private MapOfLists<String, Version> migrationsRan;

	public Migrations(ComponentContainer container, Map<String, Version> currentVersions) {
		this.container = container;
		this.currentVersions = currentVersions;
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

			if (!this.codeMigrations.isEmpty()) {
				for (CodeMigration migration : this.codeMigrations.tailSet(currentCodeMigration)) {
					migration.migrate(container, certificate);
					migrationsRan.addElement(realm, migration.getVersion());
				}
			}

			if (!this.dataMigrations.isEmpty()) {
				for (DataMigration migration : this.dataMigrations.tailSet(currentDataMigration)) {
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

	private static void logDetectedMigrations(Map<String, Version> currentVersions,
			SortedSet<DataMigration> dataMigrations, SortedSet<CodeMigration> codeMigrations) {
		for (Entry<String, Version> entry : currentVersions.entrySet()) {
			String realm = entry.getKey();
			Version currentVersion = entry.getValue();

			if (codeMigrations.isEmpty()) {
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

			if (dataMigrations.isEmpty()) {
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

	private static SortedSet<DataMigration> loadDataMigrations(Map<String, Version> currentVersions, File migrationsPath) {

		SortedSet<DataMigration> dataMigrations = new TreeSet<>((o1, o2) -> o1.getVersion().compareTo(o2.getVersion()));

		File dataDir = new File(migrationsPath, "data");
		if (dataDir.exists()) {
			DBC.PRE.assertTrue("migrations/data must be a directory!", dataDir.isDirectory());

			File[] realmMigrations = dataDir.listFiles();
			for (File realmMigration : realmMigrations) {

				DBC.PRE.assertTrue("found non directory in migrations path: " + realmMigration.getAbsolutePath(),
						realmMigration.isDirectory());
				String realm = realmMigration.getName();
				if (!currentVersions.containsKey(realm)) {
					logger.warn("Found non realm migration directory: " + realmMigration.getAbsolutePath());
					continue;
				}

				File[] migrations = realmMigration.listFiles((FileFilter) pathname -> pathname.getName().endsWith(
						".xml"));
				for (File file : migrations) {
					String name = file.getName();
					Version version = Version.valueOf(name.substring(0, name.length() - 4));
					dataMigrations.add(new DataMigration(realm, version, file));
				}
			}
		}

		return dataMigrations;
	}

	private static SortedSet<CodeMigration> loadCodeMigrations(Map<String, Version> currentVersions, File migrationsPath) {

		SortedSet<CodeMigration> codeMigrations = new TreeSet<>((o1, o2) -> o1.getVersion().compareTo(o2.getVersion()));

		File codeDir = new File(migrationsPath, "code");
		if (codeDir.exists()) {
			DBC.PRE.assertTrue("migrations/code must be a directory!", codeDir.isDirectory());

			File[] realmMigrations = codeDir.listFiles();
			for (File realmMigration : realmMigrations) {

				DBC.PRE.assertTrue("found non directory in migrations path: " + realmMigration.getAbsolutePath(),
						realmMigration.isDirectory());
				String realm = realmMigration.getName();
				if (!currentVersions.containsKey(realm)) {
					logger.warn("Found non realm migration directory: " + realmMigration.getAbsolutePath());
					continue;
				}

				File[] migrations = realmMigration.listFiles((FileFilter) pathname -> pathname.getName().endsWith(
						".xml"));
				for (File file : migrations) {
					String name = file.getName();
					Version version = Version.valueOf(name.substring(0, name.length() - 4));
					codeMigrations.add(new CodeMigration(realm, version, file));
				}
			}
		}

		return codeMigrations;
	}

	public MapOfLists<String, Version> getMigrationsToRun() {

		MapOfLists<String, Version> migrationsToRun = new MapOfLists<>();

		for (Entry<String, Version> entry : this.currentVersions.entrySet()) {
			String realm = entry.getKey();
			Version nextPossibleVersion = entry.getValue().add(0, 0, 1);
			CodeMigration currentCodeMigration = new CodeMigration(realm, nextPossibleVersion, null);
			DataMigration currentDataMigration = new DataMigration(realm, nextPossibleVersion, null);

			SortedSet<CodeMigration> codeMigrations = this.codeMigrations.tailSet(currentCodeMigration);
			for (CodeMigration codeMigration : codeMigrations) {
				if (!migrationsToRun.containsElement(realm, codeMigration.getVersion()))
					migrationsToRun.addElement(realm, codeMigration.getVersion());
			}
			SortedSet<DataMigration> dataMigrations = this.dataMigrations.tailSet(currentDataMigration);
			for (DataMigration dataMigration : dataMigrations) {
				if (!migrationsToRun.containsElement(realm, dataMigration.getVersion()))
					migrationsToRun.addElement(realm, dataMigration.getVersion());
			}
		}

		return migrationsToRun;
	}
}
