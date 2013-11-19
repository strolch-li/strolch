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
package li.strolch.runtime.agent;

import li.strolch.runtime.component.ComponentContainer;
import li.strolch.runtime.component.StrolchComponent;
import li.strolch.runtime.configuration.ComponentConfiguration;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class StrolchAgent extends StrolchComponent {

	public static final String PROP_DATA_STORE_MODE = "dataStoreMode"; //$NON-NLS-1$

	private ResourceMap resourceMap;
	private OrderMap orderMap;
	private AgentLifecycleController agentInitializer;

	/**
	 * @param container
	 * @param componentName
	 */
	public StrolchAgent(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	/**
	 * @param resourceMap
	 *            the resourceMap to set
	 */
	void setResourceMap(ResourceMap resourceMap) {
		this.resourceMap = resourceMap;
	}

	/**
	 * @param orderMap
	 *            the orderMap to set
	 */
	void setOrderMap(OrderMap orderMap) {
		this.orderMap = orderMap;
	}

	/**
	 * @return the resourceMap
	 */
	public ResourceMap getResourceMap() {
		return this.resourceMap;
	}

	/**
	 * @return the orderMap
	 */
	public OrderMap getOrderMap() {
		return this.orderMap;
	}

	@Override
	public void initialize(ComponentConfiguration configuration) {

		DataStoreMode dataStoreMode = DataStoreMode.parseDataStoreMode(configuration.getString(PROP_DATA_STORE_MODE,
				null));
		this.agentInitializer = dataStoreMode.getAgentLifecycleController(this, configuration);
		this.agentInitializer.initialize();

		super.initialize(configuration);
	}

	@Override
	public void start() {
		this.agentInitializer.start();
		super.start();
	}

	@Override
	public void stop() {
		this.agentInitializer.stop();
		super.stop();
	}

	@Override
	public void destroy() {
		this.agentInitializer.destroy();
		super.destroy();
	}
}
