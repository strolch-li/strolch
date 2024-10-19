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
package li.strolch.agent.impl;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.RealmHandler;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.exception.StrolchException;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.utils.dbc.DBC;

import java.text.MessageFormat;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static li.strolch.runtime.StrolchConstants.DEFAULT_REALM;
import static li.strolch.runtime.StrolchConstants.makeRealmKey;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultRealmHandler extends StrolchComponent implements RealmHandler {

	public static final String PROP_ENABLE_AUDIT_TRAIL = "enableAuditTrail";
	public static final String PROP_ENABLE_AUDIT_TRAIL_FOR_READ = "enableAuditTrailForRead";
	public static final String PROP_ENABLE_OBSERVER_UPDATES = "enableObserverUpdates";
	public static final String PROP_ENABLE_VERSIONING = "enableVersioning";
	public static final String PROP_TX_LOGGING_THRESHOLD_MS = "txLoggingThresholdMs";
	public static final String PREFIX_DATA_STORE_MODE = "dataStoreMode";
	public static final String PREFIX_DATA_STORE_FILE = "dataStoreFile";
	public static final String PROP_REALMS = "realms";

	protected Map<String, InternalStrolchRealm> realms;

	public DefaultRealmHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public Set<String> getRealmNames() {
		return new HashSet<>(this.realms.keySet());
	}

	@Override
	public StrolchRealm getRealm(String realm) throws StrolchException {
		DBC.PRE.assertNotEmpty("Realm name must be set!", realm);
		StrolchRealm strolchRealm = this.realms.get(realm);
		if (strolchRealm == null) {
			String msg = "No realm is configured with the name {0}";
			msg = MessageFormat.format(msg, realm);
			throw new StrolchException(msg);
		}
		return strolchRealm;
	}

	@Override
	public void setup(ComponentConfiguration configuration) {
		this.realms = new HashMap<>(1);
		String[] realms = configuration.getStringArray(PROP_REALMS, DEFAULT_REALM);
		for (String realmName : realms) {
			String dataStoreModeKey = makeRealmKey(realmName, PREFIX_DATA_STORE_MODE);
			String realmMode = configuration.getString(dataStoreModeKey, null);
			InternalStrolchRealm realm = buildRealm(realmName, realmMode);
			this.realms.put(realmName, realm);
		}
		super.setup(configuration);
	}

	protected InternalStrolchRealm buildRealm(String realmName, String realmMode) {
		DataStoreMode dataStoreMode = DataStoreMode.parseDataStoreMode(realmMode);
		return dataStoreMode.createRealm(realmName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {

		for (String realmName : this.realms.keySet()) {
			InternalStrolchRealm realm = this.realms.get(realmName);
			realm.initialize(getContainer(), configuration);
		}

		super.initialize(configuration);
	}

	Map<String, InternalStrolchRealm> getRealms() {
		return this.realms;
	}

	@Override
	public void start() throws Exception {

		PrivilegeHandler privilegeHandler = getContainer().getComponent(PrivilegeHandler.class);
		privilegeHandler.runAsAgent(ctx -> {
			for (String realmName : getRealms().keySet()) {
				InternalStrolchRealm realm = getRealms().get(realmName);
				realm.start(ctx);
			}
		});

		super.start();
	}

	@Override
	public void stop() throws Exception {
		for (String realmName : this.realms.keySet()) {
			InternalStrolchRealm realm = this.realms.get(realmName);
			try {
				realm.stop();
			} catch (Exception e) {
				logger.error("Failed to stop realm {}", realm.getRealm(), e);
			}
		}
		super.stop();
	}
}
