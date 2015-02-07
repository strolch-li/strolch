/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
import java.util.HashMap;
import java.util.Map;

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

	private static final String PATH_MIGRATIONS = "migrations";

	private Migrations migrations;
	private Map<String, Version> lastMigrations;
	private File migrationsPath;

	public MigrationsHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	public Map<String, Version> getLastMigrations() {
		return this.lastMigrations;
	}

	public Map<String, Version> getCurrentVersions(Certificate cert) {
		CurrentMigrationVersionQuery query = new CurrentMigrationVersionQuery(getContainer());
		query.doQuery(cert);
		return query.getCurrentVersions();
	}

	public MapOfLists<String, Version> queryMigrationsToRun(Certificate cert) {
		Map<String, Version> currentVersions = getCurrentVersions(cert);
		Migrations migrations = new Migrations(getContainer(), currentVersions);
		migrations.parseMigrations(this.migrationsPath);

		this.migrations = migrations;
		return this.migrations.getMigrationsToRun();
	}

	public void runMigrations(Certificate cert) {
		this.lastMigrations.clear();
		this.migrations.runMigrations(cert);
		this.lastMigrations.putAll(this.migrations.getMigrationsRan());
	}

	@Override
	public void initialize(ComponentConfiguration configuration) {

		this.lastMigrations = new HashMap<>();

		RuntimeConfiguration runtimeConf = configuration.getRuntimeConfiguration();
		this.migrationsPath = runtimeConf.getDataDir(MigrationsHandler.class.getName(), PATH_MIGRATIONS, false);
		if (this.migrationsPath.exists()) {

			CurrentMigrationVersionQuery query = new CurrentMigrationVersionQuery(getContainer());
			PrivilegeHandler privilegeHandler = getContainer().getComponent(PrivilegeHandler.class);
			QueryCurrentVersionsAction action = new QueryCurrentVersionsAction(query);
			privilegeHandler.runAsSystem(RealmHandler.SYSTEM_USER_AGENT, action);
			Map<String, Version> currentVersions = query.getCurrentVersions();

			Migrations migrations = new Migrations(getContainer(), currentVersions);
			migrations.parseMigrations(this.migrationsPath);

			this.migrations = migrations;
		}

		super.initialize(configuration);
	}

	@Override
	public void start() {

		if (this.migrations != null) {

			PrivilegeHandler privilegeHandler = getContainer().getComponent(PrivilegeHandler.class);
			RunMigrationsAction action = new RunMigrationsAction(this.migrations);

			privilegeHandler.runAsSystem(RealmHandler.SYSTEM_USER_AGENT, action);
			this.lastMigrations.putAll(this.migrations.getMigrationsRan());
		}

		super.start();
	}
}
