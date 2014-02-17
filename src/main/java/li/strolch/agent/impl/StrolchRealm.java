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

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.configuration.ComponentConfiguration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class StrolchRealm {

	protected static final Logger logger = LoggerFactory.getLogger(StrolchRealm.class);
	private String realm;

	public StrolchRealm(String realm) {
		this.realm = realm;
	}

	public String getRealm() {
		return this.realm;
	}

	public abstract DataStoreMode getMode();

	public abstract void initialize(ComponentContainer container, ComponentConfiguration configuration);

	public abstract void start();

	public abstract void stop();

	public abstract void destroy();

	public abstract StrolchTransaction openTx();

	public abstract ResourceMap getResourceMap();

	public abstract OrderMap getOrderMap();
}
