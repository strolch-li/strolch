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
package li.strolch.agent.api;

import static ch.eitchnet.utils.helper.StringHelper.DOT;

import java.util.concurrent.TimeUnit;

import li.strolch.agent.impl.DataStoreMode;
import li.strolch.agent.impl.DefaultLockHandler;
import li.strolch.model.StrolchRootElement;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.configuration.ComponentConfiguration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class StrolchRealm {

	private static final String PROP_TRY_LOCK_TIME_UNIT = "tryLockTimeUnit";
	private static final String PROP_TRY_LOCK_TIME = "tryLockTime";
	protected static final Logger logger = LoggerFactory.getLogger(StrolchRealm.class);
	private String realm;
	private LockHandler lockHandler;

	public StrolchRealm(String realm) {
		DBC.PRE.assertNotEmpty("RealmName may not be empty!", realm);
		this.realm = realm;
	}

	public String getRealm() {
		return this.realm;
	}

	public void lock(StrolchRootElement element) {
		DBC.PRE.assertNotNull("Can not lock a null pointer =)", element);
		this.lockHandler.lock(element);
	}

	public void unlock(StrolchRootElement lockedElement) {
		this.lockHandler.unlock(lockedElement);
	}

	public void initialize(ComponentContainer container, ComponentConfiguration configuration) {

		String propTryLockTimeUnit = PROP_TRY_LOCK_TIME_UNIT;
		String propTryLockTime = PROP_TRY_LOCK_TIME;
		if (!StrolchConstants.DEFAULT_REALM.equals(this.realm)) {
			propTryLockTimeUnit += DOT + this.realm;
			propTryLockTime += DOT + this.realm;
		}

		TimeUnit timeUnit = TimeUnit.valueOf(configuration.getString(propTryLockTimeUnit, TimeUnit.SECONDS.name()));
		long time = configuration.getLong(propTryLockTime, 10);
		logger.info("Using a locking try timeout of " + timeUnit.toSeconds(time) + "s");
		this.lockHandler = new DefaultLockHandler(realm, timeUnit, time);
	}

	public abstract DataStoreMode getMode();

	public abstract void start();

	public abstract void stop();

	public abstract void destroy();

	public abstract StrolchTransaction openTx();

	public abstract ResourceMap getResourceMap();

	public abstract OrderMap getOrderMap();

}
