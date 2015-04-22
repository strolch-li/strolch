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

import static li.strolch.migrations.Migration.BAG_PARAMETERS;
import static li.strolch.migrations.Migration.MIGRATIONS_ID;
import static li.strolch.migrations.Migration.MIGRATIONS_TYPE;
import static li.strolch.migrations.Migration.PARAM_CURRENT_VERSION;

import java.util.HashMap;
import java.util.Map;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.Resource;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.configuration.StrolchConfigurationException;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.Version;

public class CurrentMigrationVersionQuery {

	private ComponentContainer container;
	private Map<String, Version> currentVersions;

	/**
	 * @param container
	 */
	public CurrentMigrationVersionQuery(ComponentContainer container) {
		this.container = container;
	}

	public void doQuery(Certificate cert) {

		this.currentVersions = new HashMap<>();

		for (String realmName : this.container.getRealmNames()) {
			StrolchRealm realm = this.container.getRealm(realmName);
			try (StrolchTransaction tx = realm.openTx(cert, getClass())) {
				tx.setSuppressDoNothingLogging(true);

				Resource migrationsRes = tx.getResourceBy(MIGRATIONS_TYPE, MIGRATIONS_ID);
				if (migrationsRes == null) {
					this.currentVersions.put(realmName, Version.emptyVersion);
					continue;
				}

				StringParameter currentVersionP = migrationsRes.getParameter(BAG_PARAMETERS, PARAM_CURRENT_VERSION);
				if (currentVersionP == null) {
					this.currentVersions.put(realmName, Version.emptyVersion);
					continue;
				}

				String versionS = currentVersionP.getValue();
				if (!Version.isParseable(versionS)) {
					throw new StrolchConfigurationException("Version value " + versionS + " is not valid for "
							+ currentVersionP.getLocator());
				}

				Version version = Version.valueOf(versionS);
				this.currentVersions.put(realmName, version);
			}
		}
	}

	public Map<String, Version> getCurrentVersions() {
		return this.currentVersions;
	}

}
