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
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.configuration.ComponentConfiguration;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class TransactionalRealm extends StrolchRealm {

	private ResourceMap resourceMap;
	private OrderMap orderMap;
	private PersistenceHandler persistenceHandler;

	public TransactionalRealm(String realm) {
		super(realm);
	}

	@Override
	public DataStoreMode getMode() {
		return DataStoreMode.TRANSACTIONAL;
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
		this.resourceMap = new TransactionalResourceMap();
		this.orderMap = new TransactionalOrderMap();
		this.persistenceHandler = container.getComponent(PersistenceHandler.class);
	}

	@Override
	public void start() {

		long start = System.nanoTime();
		int nrOfOrders = 0;
		int nrOfResources = 0;

		try (StrolchTransaction tx = openTx()) {
			nrOfOrders = orderMap.getAllKeys(tx).size();
		}

		try (StrolchTransaction tx = openTx()) {
			nrOfResources = resourceMap.getAllKeys(tx).size();
		}

		long duration = System.nanoTime() - start;
		String durationS = StringHelper.formatNanoDuration(duration);
		logger.info(MessageFormat.format(
				"Initialized Transactional Maps for realm {0} took {1}.", getRealm(), durationS)); //$NON-NLS-1$
		logger.info(MessageFormat.format("There are {0} Orders", nrOfOrders)); //$NON-NLS-1$
		logger.info(MessageFormat.format("There are {0} Resources", nrOfResources)); //$NON-NLS-1$
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
