/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.agent.api.*;
import li.strolch.model.Locator;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.utils.concurrent.ElementLockingHandler;
import li.strolch.utils.dbc.DBC;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.TimeUnit;

import static li.strolch.agent.impl.DefaultRealmHandler.*;
import static li.strolch.runtime.StrolchConstants.makeRealmKey;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class InternalStrolchRealm implements StrolchRealm {

	public static final String PROP_TRY_LOCK_TIME_UNIT = "tryLockTimeUnit";
	public static final String PROP_TRY_LOCK_TIME = "tryLockTime";

	protected static final Logger logger = LoggerFactory.getLogger(InternalStrolchRealm.class);

	private final String realm;
	private ElementLockingHandler<Locator> lockHandler;
	private boolean auditTrailEnabled;
	private boolean enableModelAudits;
	private boolean enableAuditsOnRead;
	private boolean enableAuditsForAudits;
	private boolean enableVersioning;
	private boolean updateObservers;
	private ObserverHandler observerHandler;

	protected ComponentContainer container;
	private long txLoggingThresholdMs;

	public InternalStrolchRealm(String realm) {
		DBC.PRE.assertNotEmpty("RealmName may not be empty!", realm);
		this.realm = realm;
	}

	@Override
	public String getRealm() {
		return this.realm;
	}

	@Override
	public void lock(Locator locator) {
		DBC.PRE.assertNotNull("Can not lock a null pointer =)", locator);
		this.lockHandler.lock(locator);
	}

	@Override
	public void unlock(Locator locator) {
		this.lockHandler.unlock(locator);
	}

	@Override
	public void releaseLock(Locator locator) {
		this.lockHandler.releaseLock(locator);
	}

	public void initialize(ComponentContainer container, ComponentConfiguration configuration) {
		this.container = container;

		logger.info("Initializing Realm {}...", getRealm());

		// audits
		String enableAuditKey = makeRealmKey(getRealm(), PROP_ENABLE_AUDIT_TRAIL);
		this.auditTrailEnabled = configuration.getBoolean(enableAuditKey, false);
		String enableModelAuditsKey = makeRealmKey(getRealm(), PROP_ENABLE_MODEL_AUDITS);
		this.enableModelAudits = configuration.getBoolean(enableModelAuditsKey, false);
		if (this.enableModelAudits) {
			String enableAuditsOnReadKey = makeRealmKey(getRealm(), PROP_ENABLE_AUDITS_ON_READ);
			this.enableAuditsOnRead = configuration.getBoolean(enableAuditsOnReadKey, false);
		}
		String enableAuditsForAuditsKey = makeRealmKey(getRealm(), PROP_ENABLE_AUDITS_FOR_AUDITS);
		this.enableAuditsForAudits = configuration.getBoolean(enableAuditsForAuditsKey, false);

		// observer updates
		String updateObserversKey = makeRealmKey(getRealm(), PROP_ENABLE_OBSERVER_UPDATES);
		this.updateObservers = configuration.getBoolean(updateObserversKey, true);
		if (this.updateObservers) {
			this.observerHandler = new DefaultObserverHandler(container.getAgent(), this);
		}

		// lock timeout
		String propTryLockTimeUnit = makeRealmKey(this.realm, PROP_TRY_LOCK_TIME_UNIT);
		String propTryLockTime = makeRealmKey(this.realm, PROP_TRY_LOCK_TIME);
		TimeUnit timeUnit = TimeUnit.valueOf(configuration.getString(propTryLockTimeUnit, TimeUnit.SECONDS.name()));
		long time = configuration.getLong(propTryLockTime, 10L);
		this.lockHandler = new ElementLockingHandler<>(this.container.getAgent().getScheduledExecutor(), timeUnit,
				time);

		// versioning
		String enableVersioningKey = makeRealmKey(getRealm(), PROP_ENABLE_VERSIONING);
		this.enableVersioning = configuration.getBoolean(enableVersioningKey, false);

		String txLoggingThresholdMsKey = makeRealmKey(getRealm(), PROP_TX_LOGGING_THRESHOLD_MS);
		this.txLoggingThresholdMs = configuration.getLong(txLoggingThresholdMsKey, 0L);

		if (this.auditTrailEnabled) {
			logger.info("Enabling AuditTrail for realm {}", getRealm());

			if (this.enableModelAudits) {
				logger.info("Enabling model audits for realm {}", getRealm());
				if (this.enableAuditsOnRead)
					logger.info("Enabling model audits on read for realm {}", getRealm());
				else
					logger.info("Not enabling model audits on read for realm {}", getRealm());
			} else {
				logger.info("Not enabling model audits for realm {}", getRealm());
			}

			if (this.enableAuditsForAudits) {
				logger.info("Enabling audits for audits for realm {}", getRealm());
			} else {
				logger.info("Not enabling audits for audits for realm {}", getRealm());
			}
		} else {
			logger.info("AuditTrail not enabled for realm {}", getRealm());
		}

		if (this.updateObservers)
			logger.info("Enabling Observer Updates for realm {}", getRealm());
		else
			logger.info("Observer Updates not enabled for realm {}", getRealm());

		if (this.enableVersioning)
			logger.info("Enabling Versioning for realm {}", getRealm());
		else
			logger.info("Versioning not enabled for realm {}", getRealm());

		logger.info("Using a locking try timeout of {}s", timeUnit.toSeconds(time));
	}

	@Override
	public boolean isAuditTrailEnabled() {
		return this.auditTrailEnabled;
	}

	@Override
	public boolean isModelAuditsEnabled() {
		return this.enableModelAudits;
	}

	@Override
	public boolean isAuditsEnabledOnRead() {
		return this.enableAuditsOnRead;
	}

	@Override
	public boolean isAuditsForAuditsEnabled() {
		return this.enableAuditsForAudits;
	}

	@Override
	public boolean isUpdateObservers() {
		return this.updateObservers;
	}

	@Override
	public boolean isEnableVersioning() {
		return this.enableVersioning;
	}

	@Override
	public long getTxLogDurationThresholdMs() {
		return this.txLoggingThresholdMs;
	}

	@Override
	public ObserverHandler getObserverHandler() throws IllegalArgumentException {
		if (!this.updateObservers)
			throw new IllegalArgumentException("ObserverUpdates are not enabled!");
		return this.observerHandler;
	}

	public void start(PrivilegeContext privilegeContext) {

		if (this.lockHandler != null)
			this.lockHandler.start();
		if (this.observerHandler != null)
			this.observerHandler.start();
	}

	public void stop() {

		if (this.lockHandler != null)
			this.lockHandler.stop();
		if (this.observerHandler != null)
			this.observerHandler.stop();
	}

	public abstract void destroy();

	public abstract ResourceMap getResourceMap();

	public abstract OrderMap getOrderMap();

	public abstract ActivityMap getActivityMap();

	public abstract AuditTrail getAuditTrail();
}
