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

import li.strolch.agent.api.*;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class CachedRealm extends InternalStrolchRealm {

	private PersistenceHandler persistenceHandler;
	private CachedResourceMap resourceMap;
	private CachedOrderMap orderMap;
	private CachedActivityMap activityMap;
	private AuditTrail auditTrail;

	public CachedRealm(String realm) {
		super(realm);
	}

	@Override
	public DataStoreMode getMode() {
		return DataStoreMode.CACHED;
	}

	@Override
	public StrolchTransaction openTx(Certificate certificate, String action, boolean readOnly) {
		DBC.PRE.assertNotNull("Certificate must be set!", certificate); //$NON-NLS-1$
		return this.persistenceHandler.openTx(this, certificate, action, readOnly);
	}

	@Override
	public StrolchTransaction openTx(Certificate certificate, Class<?> clazz, boolean readOnly) {
		DBC.PRE.assertNotNull("Certificate must be set!", certificate); //$NON-NLS-1$
		return this.persistenceHandler.openTx(this, certificate, clazz.getName(), readOnly);
	}

	@Override
	public CachedResourceMap getResourceMap() {
		return this.resourceMap;
	}

	@Override
	public CachedOrderMap getOrderMap() {
		return this.orderMap;
	}

	@Override
	public CachedActivityMap getActivityMap() {
		return this.activityMap;
	}

	@Override
	public AuditTrail getAuditTrail() {
		return this.auditTrail;
	}

	@Override
	public void initialize(ComponentContainer container, ComponentConfiguration configuration) {
		super.initialize(container, configuration);

		this.persistenceHandler = container.getComponent(PersistenceHandler.class);
		this.resourceMap = new CachedResourceMap(this);
		this.orderMap = new CachedOrderMap(this);
		this.activityMap = new CachedActivityMap(this);

		if (isAuditTrailEnabled())
			this.auditTrail = new CachedAuditTrail();
		else
			this.auditTrail = new NoStrategyAuditTrail(getRealm());
	}

	@Override
	public void start(PrivilegeContext privilegeContext) {
		super.start(privilegeContext);
		new CachedRealmLoader(this, this.persistenceHandler, privilegeContext).load(getRealm());
	}

	@Override
	public void destroy() {
		// 
	}
}
