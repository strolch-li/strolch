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

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.inmemory.InMemoryPersistence;
import li.strolch.runtime.configuration.ComponentConfiguration;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class EmptyRealm extends StrolchRealm {

	private ResourceMap resourceMap;
	private OrderMap orderMap;
	private PersistenceHandler persistenceHandler;

	public EmptyRealm(String realm) {
		super(realm);
	}

	@Override
	public DataStoreMode getMode() {
		return DataStoreMode.EMPTY;
	}

	@Override
	public StrolchTransaction openTx() {
		return this.persistenceHandler.openTx(this);
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
	public void initialize(ComponentContainer container, ComponentConfiguration configuration) {
		super.initialize(container, configuration);
		this.persistenceHandler = new InMemoryPersistence();
		this.resourceMap = new TransactionalResourceMap();
		this.orderMap = new TransactionalOrderMap();
	}

	@Override
	public void start() {
		logger.info(MessageFormat.format("Initialized EMPTY Realm {0}", getRealm())); //$NON-NLS-1$
	}

	@Override
	public void stop() {
		// 
	}

	@Override
	public void destroy() {
		// 
	}
}
