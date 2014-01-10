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
package li.strolch.persistence.api;

import li.strolch.persistence.inmemory.InMemoryTransaction;
import li.strolch.runtime.agent.api.OrderMap;
import li.strolch.runtime.agent.api.ResourceMap;
import li.strolch.runtime.agent.impl.StrolchRealm;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractTransaction implements StrolchTransaction {

	protected static final Logger logger = LoggerFactory.getLogger(InMemoryTransaction.class);
	private StrolchRealm realm;

	/**
	 * @param realm
	 */
	public AbstractTransaction(StrolchRealm realm) {
		this.realm = realm;
	}

	/**
	 * @return the realm
	 */
	protected StrolchRealm getRealm() {
		return this.realm;
	}

	@Override
	public ResourceMap getResourceMap() {
		return this.realm.getResourceMap();
	}

	@Override
	public OrderMap getOrderMap() {
		return this.realm.getOrderMap();
	}
}
