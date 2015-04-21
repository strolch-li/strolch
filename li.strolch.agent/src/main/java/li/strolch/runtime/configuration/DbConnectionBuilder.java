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

import java.sql.Connection;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import javax.sql.DataSource;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.persistence.api.StrolchPersistenceException;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class DbConnectionBuilder {

	private ComponentContainer container;
	private ComponentConfiguration configuration;

	/**
	 * @param container
	 * @param persistenceHandlerConfiguration
	 */
	public DbConnectionBuilder(ComponentContainer container, ComponentConfiguration persistenceHandlerConfiguration) {
		this.container = container;
		this.configuration = persistenceHandlerConfiguration;
	}

	public Map<String, DataSource> build() {

		Map<String, DataSource> dsMap = new HashMap<>();

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

			DataSource ds = build(realmName, dbUrl, username, password, null);

			dsMap.put(realmName, ds);
		}

		return dsMap;
	}

	protected abstract DataSource build(String realm, String url, String username, String password, Properties proops);

	protected void validateConnection(DataSource ds) {
		try (Connection con = ds.getConnection()) {
			con.commit();
		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to validate connection to " + ds, e);
		}
	}
}
