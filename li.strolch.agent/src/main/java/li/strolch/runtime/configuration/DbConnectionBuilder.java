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
package li.strolch.runtime.configuration;

import static ch.eitchnet.db.DbConstants.PROP_DB_PASSWORD;
import static ch.eitchnet.db.DbConstants.PROP_DB_URL;
import static ch.eitchnet.db.DbConstants.PROP_DB_USERNAME;
import static li.strolch.runtime.StrolchConstants.makeRealmKey;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchRealm;
import ch.eitchnet.db.DbConnectionInfo;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DbConnectionBuilder {

	private ComponentContainer container;
	private ComponentConfiguration configuration;

	/**
	 * 
	 * @param container
	 * @param persistenceHandlerConfiguration
	 */
	public DbConnectionBuilder(ComponentContainer container, ComponentConfiguration persistenceHandlerConfiguration) {
		this.container = container;
		this.configuration = persistenceHandlerConfiguration;
	}

	public Map<String, DbConnectionInfo> build() {

		Map<String, DbConnectionInfo> connetionInfoMap = new HashMap<>();

		Set<String> realmNames = container.getRealmNames();
		for (String realmName : realmNames) {

			StrolchRealm realm = container.getRealm(realmName);
			if (realm.getMode().isTransient())
				continue;

			String dbUrlKey = makeRealmKey(realmName, PROP_DB_URL);
			String dbUsernameKey = makeRealmKey(realmName, PROP_DB_USERNAME);
			String dbPasswordKey = makeRealmKey(realmName, PROP_DB_PASSWORD);

			String dbUrl = configuration.getString(dbUrlKey, null);
			String username = configuration.getString(dbUsernameKey, null);
			String password = configuration.getString(dbPasswordKey, null);

			DbConnectionInfo connectionInfo = new DbConnectionInfo(realmName, dbUrl);
			connectionInfo.setUsername(username);
			connectionInfo.setPassword(password);

			connetionInfoMap.put(realmName, connectionInfo);
		}

		return connetionInfoMap;
	}
}
