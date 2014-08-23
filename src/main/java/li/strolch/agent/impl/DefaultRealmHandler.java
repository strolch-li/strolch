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

import static ch.eitchnet.utils.helper.StringHelper.DOT;

import java.text.MessageFormat;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.RealmHandler;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.exception.StrolchException;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.privilege.PrivilegeHandler;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultRealmHandler extends StrolchComponent implements RealmHandler {

	public static final String PROP_ENABLE_AUDIT_TRAIL = "enableAuditTrail"; //$NON-NLS-1$
	public static final String PREFIX_DATA_STORE_MODE = "dataStoreMode"; //$NON-NLS-1$
	public static final String PROP_REALMS = "realms"; //$NON-NLS-1$

	protected Map<String, StrolchRealm> realms;

	/**
	 * @param container
	 * @param componentName
	 */
	public DefaultRealmHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public Set<String> getRealmNames() {
		return new HashSet<>(this.realms.keySet());
	}

	@Override
	public StrolchRealm getRealm(String realm) throws StrolchException {
		DBC.PRE.assertNotEmpty("Realm name must be set!", realm); //$NON-NLS-1$
		StrolchRealm strolchRealm = this.realms.get(realm);
		if (strolchRealm == null) {
			String msg = "No realm is configured with the name {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, realm);
			throw new StrolchException(msg);
		}
		return strolchRealm;
	}

	@Override
	public void setup(ComponentConfiguration configuration) {

		this.realms = new HashMap<>();
		String[] realms = configuration.getStringArray(PROP_REALMS, StrolchConstants.DEFAULT_REALM);
		for (String realmName : realms) {
			String dataStoreModeKey = makeRealmKey(realmName, PREFIX_DATA_STORE_MODE);
			String realmMode = configuration.getString(dataStoreModeKey, null);
			DataStoreMode dataStoreMode = DataStoreMode.parseDataStoreMode(realmMode);
			StrolchRealm realm = dataStoreMode.createRealm(realmName);
			this.realms.put(realmName, realm);
		}
		super.setup(configuration);
	}

	public static String makeRealmKey(String realmName, String key) {
		String realmKey = key;
		if (!realmName.equals(StrolchConstants.DEFAULT_REALM))
			realmKey += DOT + realmName;
		return realmKey;
	}

	@Override
	public void initialize(ComponentConfiguration configuration) {

		for (String realmName : this.realms.keySet()) {
			StrolchRealm realm = this.realms.get(realmName);
			realm.initialize(getContainer(), configuration);
		}

		super.initialize(configuration);
	}

	Map<String, StrolchRealm> getRealms() {
		return this.realms;
	}

	@Override
	public void start() {

		PrivilegeHandler privilegeHandler = getContainer().getComponent(PrivilegeHandler.class);
		privilegeHandler.runAsSystem("agent", new StartRealms(this));

		super.start();
	}

	@Override
	public void stop() {
		for (String realmName : this.realms.keySet()) {
			StrolchRealm realm = this.realms.get(realmName);
			realm.stop();
		}
		super.stop();
	}
}
