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

import static li.strolch.db.DbConstants.*;
import static li.strolch.runtime.StrolchConstants.DEFAULT_REALM;
import static li.strolch.runtime.StrolchConstants.makeRealmKey;
import static li.strolch.utils.helper.StringHelper.*;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.persistence.api.StrolchPersistenceException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class DbConnectionBuilder {

	private static final String PROP_DB_POOL_PREFIX = "db.pool";

	protected static final Logger logger = LoggerFactory.getLogger(DbConnectionBuilder.class);

	private final ComponentContainer container;
	private final ComponentConfiguration configuration;

	public DbConnectionBuilder(ComponentContainer container, ComponentConfiguration persistenceHandlerConfiguration) {
		this.container = container;
		this.configuration = persistenceHandlerConfiguration;
	}

	public Map<String, DataSource> build() {

		Map<String, DataSource> dsMap = new HashMap<>();

		Set<String> realmNames = this.container.getRealmNames();
		for (String realmName : realmNames) {

			StrolchRealm realm = this.container.getRealm(realmName);
			if (realm.getMode().isTransient())
				continue;

			String dbUseEnvKey = makeRealmKey(realmName, PROP_USE_ENV, false);
			boolean dbUseEnv = this.configuration.getBoolean(dbUseEnvKey, false);
			if (dbUseEnv)
				logger.info("Configuration specifies to use environment variables to configure DB access...");

			String dbUrlKey = makeRealmKey(realmName, PROP_DB_URL, dbUseEnv);
			String dbUsernameKey = makeRealmKey(realmName, PROP_DB_USERNAME, dbUseEnv);
			String dbPasswordKey = makeRealmKey(realmName, PROP_DB_PASSWORD, dbUseEnv);

			String dbIgnoreRealmKey = makeRealmKey(realmName, PROP_DB_IGNORE_REALM, false);
			boolean dbIgnoreRealm = this.configuration.getBoolean(dbIgnoreRealmKey, false);
			if (dbIgnoreRealm) {
				logger.info("[{}] Ignoring any DB configuration for Realm {}", realm, realmName);
				continue;
			}

			String dbUrl = getConfigString(dbUrlKey, dbUseEnv);
			String username = getConfigString(dbUsernameKey, dbUseEnv);
			String password = getConfigString(dbPasswordKey, dbUseEnv, true);

			if (this.configuration.getBoolean(PROP_DB_ALLOW_HOST_OVERRIDE_ENV, false) //
					&& System.getProperties().containsKey(PROP_DB_HOST_OVERRIDE)) {
				dbUrl = overridePostgresqlHost(realm.getRealm(), dbUrl, dbUseEnv);
			}

			// find any pool configuration values
			Map<String, String> properties = dbUseEnv ? System.getenv() : this.configuration.getAsMap();
			String dbPoolPrefix = dbUseEnv ? PROP_DB_POOL_PREFIX.replace(DOT, UNDERLINE).toUpperCase() :
					PROP_DB_POOL_PREFIX;
			Properties props = new Properties();
			for (String key : properties.keySet()) {
				if (!key.startsWith(dbPoolPrefix))
					continue;

				// TODO we should change how properties for realms are configured
				// since defaultRealm does not have to be on the key, we need this hack:
				String[] segments = key.split(dbUseEnv ? UNDERLINE : "\\.");
				String poolKey;
				String foundRealm;
				if (segments.length == 4) {
					// ends with realm
					foundRealm = segments[3];
				} else if (segments.length == 3) {
					// default realm
					foundRealm = DEFAULT_REALM;
				} else {
					throw new IllegalArgumentException("Can't detect realm of this property: " + key);
				}
				// see if this is our realm
				if (!foundRealm.equals(realmName))
					continue;

				poolKey = segments[2];
				String value = properties.get(key);
				props.setProperty(poolKey, value);
			}

			DataSource dataSource = build(realmName, dbUrl, username, password, props);
			dsMap.put(realmName, dataSource);
		}

		return dsMap;
	}

	private String getConfigString(String dbKey, boolean useEnv) {
		return getConfigString(dbKey, useEnv, false);
	}

	private String getConfigString(String dbKey, boolean useEnv, boolean isSecret) {
		if (!useEnv) {
			if (isSecret)
				return this.configuration.getSecret(dbKey);
			return this.configuration.getString(dbKey, null);
		}

		String value = System.getenv(dbKey);
		if (isEmpty(value))
			throw new IllegalStateException("Missing environment variable " + dbKey);
		return value;
	}

	public static String overridePostgresqlHost(String realmName, String dbUrl) {
		return overridePostgresqlHost(realmName, dbUrl, false);
	}

	public static String overridePostgresqlHost(String realmName, String dbUrl, boolean useEnv) {
		String hostOverride;
		if (useEnv) {
			if (!System.getenv().containsKey(ENV_DB_HOST_OVERRIDE))
				return dbUrl;
			hostOverride = System.getenv(ENV_DB_HOST_OVERRIDE);
		} else {
			if (!System.getProperties().containsKey(PROP_DB_HOST_OVERRIDE))
				return dbUrl;
			hostOverride = System.getProperty(PROP_DB_HOST_OVERRIDE);
		}

		if (!dbUrl.startsWith("jdbc:postgresql://"))
			throw new IllegalStateException("DB URL is invalid: " + dbUrl);

		String tmp = dbUrl.substring("jdbc:postgresql://".length());
		String host = tmp.substring(0, tmp.indexOf('/'));
		String dbName = tmp.substring(tmp.indexOf('/'));

		if (host.equals(hostOverride))
			return dbUrl;

		logger.warn("[{}] Replacing db host {} with override {}", realmName, host, hostOverride);
		dbUrl = "jdbc:postgresql://" + hostOverride + dbName;
		logger.warn("[{}] DB URL is now {}", realmName, dbUrl);
		return dbUrl;
	}

	protected abstract DataSource build(String realm, String url, String username, String password, Properties props);

	protected void validateConnection(DataSource ds) {
		try (Connection con = ds.getConnection()) {
			con.commit();
		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to validate connection to " + ds, e);
		}
	}
}
