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

import java.text.MessageFormat;

import li.strolch.agent.api.*;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class EmptyRealm extends InternalStrolchRealm {

	private ResourceMap resourceMap;
	private OrderMap orderMap;
	private ActivityMap activityMap;
	private AuditTrail auditTrail;

	public EmptyRealm(String realm) {
		super(realm);
	}

	@Override
	public DataStoreMode getMode() {
		return DataStoreMode.EMPTY;
	}

	@Override
	public StrolchTransaction openTx(Certificate certificate, String action, boolean readOnly) {
		DBC.PRE.assertNotNull("Certificate must be set!", certificate);
		return new TransientTransaction(this.container, this, certificate, action, readOnly);
	}

	@Override
	public StrolchTransaction openTx(Certificate certificate, Class<?> clazz, boolean readOnly) {
		DBC.PRE.assertNotNull("Certificate must be set!", certificate);
		return new TransientTransaction(this.container, this, certificate, clazz.getName(),readOnly);
	}

	@Override
	public ResourceMap getResourceMap() {
		return this.resourceMap;
	}

	@Override
	public OrderMap getOrderMap() {
		return this.orderMap;
	}

	@Override
	public ActivityMap getActivityMap() {
		return this.activityMap;
	}

	@Override
	public AuditTrail getAuditTrail() {
		return this.auditTrail;
	}

	@Override
	public void initialize(ComponentContainer container, ComponentConfiguration configuration) {
		super.initialize(container, configuration);
		this.resourceMap = new TransientResourceMap();
		this.orderMap = new TransientOrderMap();
		this.activityMap = new TransientActivityMap();

		if (isAuditTrailEnabled())
			this.auditTrail = new TransientAuditTrail();
		else
			this.auditTrail = new NoStrategyAuditTrail();
	}

	@Override
	public void start(PrivilegeContext privilegeContext) {
		super.start(privilegeContext);
		logger.info(MessageFormat.format("Initialized EMPTY Realm {0}", getRealm()));
	}

	@Override
	public void destroy() {
		// 
	}
}
