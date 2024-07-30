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

import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;

import java.io.File;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.TimeUnit;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.Tags;
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogMessageState;
import li.strolch.model.log.LogSeverity;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.utils.Version;
import li.strolch.utils.collections.MapOfLists;

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
	public void initialize(ComponentConfiguration configuration) throws Exception {

		this.runMigrationsOnStart = configuration.getBoolean(PROP_RUN_MIGRATIONS_ON_START, Boolean.FALSE);
		this.verbose = configuration.getBoolean(PROP_VERBOSE, Boolean.FALSE);
		boolean pollMigrations = configuration.getBoolean(PROP_POLL_MIGRATIONS, Boolean.FALSE);
		int pollWait = configuration.getInt(PROP_POLL_WAIT, 5);

		RuntimeConfiguration runtimeConf = configuration.getRuntimeConfiguration();
		this.migrationsPath = runtimeConf.getDataDir(MigrationsHandler.class.getName(), PATH_MIGRATIONS, false);
		if (this.runMigrationsOnStart && this.migrationsPath.exists()) {

			Migrations migrations = new Migrations(getContainer(), getContainer().getRealmNames(), this.verbose);
			migrations.parseMigrations(this.migrationsPath);

			this.migrations = migrations;
		}

		if (pollMigrations) {
			this.migrationTimer = new Timer("MigrationTimer", true);
			long checkInterval = TimeUnit.MINUTES.toMillis(pollWait);
			this.migrationTimer.schedule(new MigrationPollTask(), checkInterval, checkInterval);
		}

		super.initialize(configuration);
	}

	@Override
	public void start() throws Exception {

		if (this.runMigrationsOnStart && this.migrations != null) {

			CurrentMigrationVersionQuery query = new CurrentMigrationVersionQuery(getContainer());
			PrivilegeHandler privilegeHandler = getContainer().getComponent(PrivilegeHandler.class);
			privilegeHandler.runAsAgent(ctx -> query.doQuery(ctx.getCertificate()));
			Map<String, MigrationVersion> currentVersions = query.getCurrentVersions();
			privilegeHandler.runAsAgent(ctx -> migrations.runMigrations(ctx.getCertificate(), currentVersions));
		}

		super.start();
	}

	@Override
	public void stop() throws Exception {

		if (this.migrationTimer != null) {
			this.migrationTimer.cancel();
		}

		this.migrationTimer = null;

		super.stop();
	}

	/**
	 * Simpler {@link TimerTask} to check for sessions which haven't been active for {@link #PROP_POLL_WAIT} minutes.
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

			try {

				Migrations migrations = new Migrations(getContainer(), getContainer().getRealmNames(),
						MigrationsHandler.this.verbose);
				migrations.parseMigrations(MigrationsHandler.this.migrationsPath);

				CurrentMigrationVersionQuery query = new CurrentMigrationVersionQuery(getContainer());
				PrivilegeHandler privilegeHandler = getContainer().getComponent(PrivilegeHandler.class);
				privilegeHandler.runAsAgent(ctx -> query.doQuery(ctx.getCertificate()));
				Map<String, MigrationVersion> currentVersions = query.getCurrentVersions();

				MigrationsHandler.this.migrations = migrations;
				if (migrations.getMigrationsToRun(currentVersions).isEmpty()) {
					if (verbose)
						logger.info("There are no migrations required at the moment!");
				} else {
					privilegeHandler.runAsAgent(ctx -> migrations.runMigrations(ctx.getCertificate(), currentVersions));
				}

			} catch (Exception e) {
				logger.error("Failed to run migrations!", e);

				if (getContainer().hasComponent(OperationsLog.class)) {
					getComponent(OperationsLog.class).addMessage(new LogMessage(Tags.AGENT, SYSTEM_USER_AGENT,
							getLocator().append(MigrationPollTask.class.getName()), LogSeverity.Exception,
							LogMessageState.Information, ResourceBundle.getBundle("strolch-service"),
							"execution.handler.failed.executed").withException(e).value("reason", e));
				}
			}
		}
	}
}
