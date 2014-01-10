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

import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.persistence.api.StrolchTransaction;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class TransactionalElementMapHandler extends InMemoryElementMapHandler {

	/**
	 * @param container
	 * @param componentName
	 */
	public TransactionalElementMapHandler(ComponentContainerImpl container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void start() {

		for (String realm : this.realms.keySet()) {

			long start = System.nanoTime();
			int nrOfOrders = 0;
			int nrOfResources = 0;

			StrolchRealm strolchRealm = this.realms.get(realm);
			OrderMap orderMap = strolchRealm.getOrderMap();
			ResourceMap resourceMap = strolchRealm.getResourceMap();

			try (StrolchTransaction tx = strolchRealm.openTx()) {
				nrOfOrders = orderMap.getAllKeys(tx).size();
			}

			try (StrolchTransaction tx = strolchRealm.openTx()) {
				nrOfResources = resourceMap.getAllKeys(tx).size();
			}

			long duration = System.nanoTime() - start;
			String durationS = StringHelper.formatNanoDuration(duration);
			logger.info(MessageFormat
					.format("Initialized Transactional Maps for realm {0} took {1}.", realm, durationS)); //$NON-NLS-1$
			logger.info(MessageFormat.format("There are {0} Orders", nrOfOrders)); //$NON-NLS-1$
			logger.info(MessageFormat.format("There are {0} Resources", nrOfResources)); //$NON-NLS-1$
		}

		super.start();
	}
}
