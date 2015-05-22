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
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.TimeUnit;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.RealmHandler;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import li.strolch.runtime.privilege.PrivilegeHandler;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.Version;
import ch.eitchnet.utils.collections.MapOfLists;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class MigrationsHandler extends StrolchComponent {

	private static final String PROP_VERBOSE = "verbose";
	private static final String PROP_POLL_MIGRATIONS = "pollMigrations";
	private static final String PROP_POLL_WAIT = "pollWait";
	private static final String PROP_RUN_MIGRATIONS_ON_START = "runMigrationsOnStart";
	private static final String PATH_MIGRATIONS = "migrations";

	private boolean runMigrationsOnStart;
	private boolean verbose;

	private Migrations migrations;
	private File migrationsPath;

	private Timer migrationTimer;
	private boolean pollMigrations;
	private int pollWait;

	public MigrationsHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	public MapOfLists<String, Version> getLastMigrations() {
		if (this.migrations == null)
			return new MapOfLists<>();
		return this.migrations.getMigrationsRan();
	}

	public Map<String, MigrationVersion> getCurrentVersions(Certificate cert) {
		CurrentMigrationVersionQuery query = new CurrentMigrationVersionQuery(getContainer());
		query.doQuery(cert);
		return query.getCurrentVersions();
	}

	public MapOfLists<String, Version> queryMigrationsToRun(Certificate cert) {
		if (!this.migrationsPath.isDirectory())
			return new MapOfLists<>();

		Migrations migrations = new Migrations(getContainer(), getContainer().getRealmNames(), this.verbose);
		migrations.parseMigrations(this.migrationsPath);

		Map<String, MigrationVersion> currentVersions = getCurrentVersions(cert);
		this.migrations = migrations;
		return this.migrations.getMigrationsToRun(currentVersions);
	}

	public void runMigrations(Certificate cert) {
		if (!this.migrationsPath.isDirectory())
			return;

		Migrations migrations = new Migrations(getContainer(), getContainer().getRealmNames(), this.verbose);
		migrations.parseMigrations(this.migrationsPath);

		Map<String, MigrationVersion> currentVersions = getCurrentVersions(cert);
		this.migrations.runMigrations(cert, currentVersions);
	}

	public void runCodeMigrations(Certificate cert, MapOfLists<String, CodeMigration> codeMigrationsByRealm) {
		Map<String, MigrationVersion> currentVersions = getCurrentVersions(cert);
		Migrations migrations = new Migrations(getContainer(), getContainer().getRealmNames(), this.verbose);
		this.migrations = migrations;
		migrations.runCodeMigrations(cert, currentVersions, codeMigrationsByRealm);
	}

	public boolean isVerbose() {
		return this.verbose;
	}

	@Override
	public void initialize(ComponentConfiguration configuration) {

		this.runMigrationsOnStart = configuration.getBoolean(PROP_RUN_MIGRATIONS_ON_START, Boolean.FALSE);
		this.verbose = configuration.getBoolean(PROP_VERBOSE, Boolean.FALSE);
		this.pollMigrations = configuration.getBoolean(PROP_POLL_MIGRATIONS, Boolean.FALSE);
		this.pollWait = configuration.getInt(PROP_POLL_WAIT, 5);

		RuntimeConfiguration runtimeConf = configuration.getRuntimeConfiguration();
		this.migrationsPath = runtimeConf.getDataDir(MigrationsHandler.class.getName(), PATH_MIGRATIONS, false);
		if (this.runMigrationsOnStart && this.migrationsPath.exists()) {

			Migrations migrations = new Migrations(getContainer(), getContainer().getRealmNames(), this.verbose);
			migrations.parseMigrations(this.migrationsPath);

			this.migrations = migrations;
		}

		if (this.pollMigrations) {
			this.migrationTimer = new Timer("MigrationTimer", true); //$NON-NLS-1$
			long checkInterval = TimeUnit.MINUTES.toMillis(pollWait);
			this.migrationTimer.schedule(new MigrationPollTask(), checkInterval, checkInterval);
		}

		super.initialize(configuration);
	}

	@Override
	public void start() {

		if (this.runMigrationsOnStart && this.migrations != null) {

			CurrentMigrationVersionQuery query = new CurrentMigrationVersionQuery(getContainer());
			QueryCurrentVersionsAction queryAction = new QueryCurrentVersionsAction(query);
			PrivilegeHandler privilegeHandler = getContainer().getComponent(PrivilegeHandler.class);
			privilegeHandler.runAsSystem(RealmHandler.SYSTEM_USER_AGENT, queryAction);
			Map<String, MigrationVersion> currentVersions = query.getCurrentVersions();

			RunMigrationsAction action = new RunMigrationsAction(this.migrations, currentVersions);

			privilegeHandler.runAsSystem(RealmHandler.SYSTEM_USER_AGENT, action);
		}

		super.start();
	}

	@Override
	public void stop() {

		if (this.migrationTimer != null) {
			this.migrationTimer.cancel();
		}

		this.migrationTimer = null;

		super.stop();
	}

	/**
	 * Simpler {@link TimerTask} to check for sessions which haven't been active for
	 * {@link DefaultStrolchSessionHandler#PARAM_SESSION_TTL_MINUTES} minutes.
	 * 
	 * @author Robert von Burg <eitch@eitchnet.ch>
	 */
	private class MigrationPollTask extends TimerTask {

		@Override
		public void run() {

			if (!MigrationsHandler.this.migrationsPath.isDirectory()) {
				if (verbose)
					logger.info("There are no migrations required at the moment!");
				return;
			}

			Migrations migrations = new Migrations(getContainer(), getContainer().getRealmNames(),
					MigrationsHandler.this.verbose);
			migrations.parseMigrations(MigrationsHandler.this.migrationsPath);

			CurrentMigrationVersionQuery query = new CurrentMigrationVersionQuery(getContainer());
			QueryCurrentVersionsAction queryAction = new QueryCurrentVersionsAction(query);
			PrivilegeHandler privilegeHandler = getContainer().getComponent(PrivilegeHandler.class);
			privilegeHandler.runAsSystem(RealmHandler.SYSTEM_USER_AGENT, queryAction);
			Map<String, MigrationVersion> currentVersions = query.getCurrentVersions();

			MigrationsHandler.this.migrations = migrations;
			if (migrations.getMigrationsToRun(currentVersions).isEmpty()) {
				if (verbose)
					logger.info("There are no migrations required at the moment!");
			} else {
				RunMigrationsAction runMigrationsAction = new RunMigrationsAction(MigrationsHandler.this.migrations,
						currentVersions);
				privilegeHandler.runAsSystem(RealmHandler.SYSTEM_USER_AGENT, runMigrationsAction);
			}
		}
	}
}
