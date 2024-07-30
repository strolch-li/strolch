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

import static java.util.concurrent.TimeUnit.NANOSECONDS;
import static li.strolch.migrations.Migration.*;
import static li.strolch.model.StrolchModelConstants.BAG_PARAMETERS;

import java.util.HashMap;
import java.util.Map;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.Resource;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.configuration.StrolchConfigurationException;
import li.strolch.utils.Version;

public class CurrentMigrationVersionQuery {

	private final ComponentContainer container;
	private Map<String, MigrationVersion> currentVersions;

	public CurrentMigrationVersionQuery(ComponentContainer container) {
		this.container = container;
	}

	public void doQuery(Certificate cert) {

		this.currentVersions = new HashMap<>();

		for (String realmName : this.container.getRealmNames()) {
			StrolchRealm realm = this.container.getRealm(realmName);
			try (StrolchTransaction tx = realm.openTx(cert, getClass(), false).silentThreshold(1, NANOSECONDS)) {

				Resource migrationsRes = tx.getResourceBy(MIGRATIONS_TYPE, MIGRATIONS_ID);
				if (migrationsRes == null) {
					this.currentVersions
							.put(realmName, new MigrationVersion(Version.emptyVersion, Version.emptyVersion));
					continue;
				}

				StringParameter currentDataVersionP = migrationsRes
						.getParameter(BAG_PARAMETERS, PARAM_CURRENT_DATA_VERSION);
				StringParameter currentCodeVersionP = migrationsRes
						.getParameter(BAG_PARAMETERS, PARAM_CURRENT_CODE_VERSION);

				if (currentDataVersionP == null && currentCodeVersionP == null) {
					this.currentVersions
							.put(realmName, new MigrationVersion(Version.emptyVersion, Version.emptyVersion));
				} else if (currentDataVersionP == null) {
					Version codeVersion = getVersionFromParam(currentCodeVersionP);
					this.currentVersions.put(realmName, new MigrationVersion(Version.emptyVersion, codeVersion));
				} else if (currentCodeVersionP == null) {
					Version dataVersion = getVersionFromParam(currentDataVersionP);
					this.currentVersions.put(realmName, new MigrationVersion(dataVersion, Version.emptyVersion));
				} else {
					Version dataVersion = getVersionFromParam(currentDataVersionP);
					Version codeVersion = getVersionFromParam(currentCodeVersionP);
					this.currentVersions.put(realmName, new MigrationVersion(dataVersion, codeVersion));
				}
			}
		}
	}

	private Version getVersionFromParam(StringParameter versionP) {
		String versionS = versionP.getValue();
		if (!Version.isParseable(versionS)) {
			throw new StrolchConfigurationException(
					"Version value " + versionS + " is not valid for " + versionP.getLocator());
		}

		return Version.valueOf(versionS);
	}

	public Map<String, MigrationVersion> getCurrentVersions() {
		return this.currentVersions;
	}
}
