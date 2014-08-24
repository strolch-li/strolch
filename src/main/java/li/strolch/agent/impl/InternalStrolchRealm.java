/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.agent.impl;

import java.text.MessageFormat;
import java.util.concurrent.TimeUnit;

import li.strolch.agent.api.AuditTrail;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.LockHandler;
import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.StrolchRootElement;
import li.strolch.runtime.configuration.ComponentConfiguration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class InternalStrolchRealm implements StrolchRealm {

	public static final String PROP_TRY_LOCK_TIME_UNIT = "tryLockTimeUnit"; //$NON-NLS-1$
	public static final String PROP_TRY_LOCK_TIME = "tryLockTime"; //$NON-NLS-1$
	protected static final Logger logger = LoggerFactory.getLogger(StrolchRealm.class);
	private String realm;
	private LockHandler lockHandler;
	private boolean auditTrailEnabled;
	private boolean auditTrailEnabledForRead;

	public InternalStrolchRealm(String realm) {
		DBC.PRE.assertNotEmpty("RealmName may not be empty!", realm); //$NON-NLS-1$
		this.realm = realm;
	}

	@Override
	public String getRealm() {
		return this.realm;
	}

	@Override
	public void lock(StrolchRootElement element) {
		DBC.PRE.assertNotNull("Can not lock a null pointer =)", element); //$NON-NLS-1$
		this.lockHandler.lock(element);
	}

	@Override
	public void unlock(StrolchRootElement lockedElement) {
		this.lockHandler.unlock(lockedElement);
	}

	public void initialize(ComponentContainer container, ComponentConfiguration configuration) {

		String propTryLockTimeUnit = DefaultRealmHandler.makeRealmKey(this.realm, PROP_TRY_LOCK_TIME_UNIT);
		String propTryLockTime = DefaultRealmHandler.makeRealmKey(this.realm, PROP_TRY_LOCK_TIME);

		String enableAuditKey = DefaultRealmHandler.makeRealmKey(getRealm(),
				DefaultRealmHandler.PROP_ENABLE_AUDIT_TRAIL);
		this.auditTrailEnabled = configuration.getBoolean(enableAuditKey, Boolean.FALSE);

		String enableAuditForReadKey = DefaultRealmHandler.makeRealmKey(getRealm(),
				DefaultRealmHandler.PROP_ENABLE_AUDIT_TRAIL_FOR_READ);
		this.auditTrailEnabledForRead = configuration.getBoolean(enableAuditForReadKey, Boolean.FALSE);

		TimeUnit timeUnit = TimeUnit.valueOf(configuration.getString(propTryLockTimeUnit, TimeUnit.SECONDS.name()));
		long time = configuration.getLong(propTryLockTime, 10);
		logger.info(MessageFormat.format("Using a locking try timeout of {0}s", timeUnit.toSeconds(time))); //$NON-NLS-1$
		this.lockHandler = new DefaultLockHandler(this.realm, timeUnit, time);
	}

	/**
	 * @return the auditTrailEnabled
	 */
	@Override
	public boolean isAuditTrailEnabled() {
		return this.auditTrailEnabled;
	}

	/**
	 * @return the auditTrailEnabledForRead
	 */
	@Override
	public boolean isAuditTrailEnabledForRead() {
		return this.auditTrailEnabledForRead;
	}

	public abstract void start(PrivilegeContext privilegeContext);

	public abstract void stop();

	public abstract void destroy();

	public abstract ResourceMap getResourceMap();

	public abstract OrderMap getOrderMap();

	public abstract AuditTrail getAuditTrail();
}
