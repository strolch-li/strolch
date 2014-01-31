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

import java.util.Collections;
import java.util.Set;

import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.xml.StrolchElementListener;
import li.strolch.persistence.api.StrolchTransaction;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class InMemoryElementListener implements StrolchElementListener {

	private boolean addOrders;
	private boolean addResources;
	private boolean updateOrders;
	private boolean updateResources;
	private Set<String> orderTypes;
	private Set<String> resourceTypes;

	private StrolchTransaction tx;
	private ResourceMap resourceMap;
	private OrderMap orderMap;

	public InMemoryElementListener(StrolchTransaction tx) {
		this.tx = tx;
		this.resourceMap = tx.getResourceMap();
		this.orderMap = tx.getOrderMap();

		this.addResources = true;
		this.addOrders = true;
		this.updateResources = true;
		this.updateOrders = true;
		this.orderTypes = Collections.emptySet();
		this.resourceTypes = Collections.emptySet();
	}

	/**
	 * @param addResources
	 *            the addResources to set
	 */
	public void setAddResources(boolean addResources) {
		this.addResources = addResources;
	}

	/**
	 * @param addOrders
	 *            the addOrders to set
	 */
	public void setAddOrders(boolean addOrders) {
		this.addOrders = addOrders;
	}

	/**
	 * @param updateResources
	 *            the updateResources to set
	 */
	public void setUpdateResources(boolean updateResources) {
		this.updateResources = updateResources;
	}

	/**
	 * @param updateOrders
	 *            the updateOrders to set
	 */
	public void setUpdateOrders(boolean updateOrders) {
		this.updateOrders = updateOrders;
	}

	/**
	 * @param orderTypes
	 *            the orderTypes to set
	 */
	public void setOrderTypes(Set<String> orderTypes) {
		this.orderTypes = orderTypes;
	}

	/**
	 * @param resourceTypes
	 *            the resourceTypes to set
	 */
	public void setResourceTypes(Set<String> resourceTypes) {
		this.resourceTypes = resourceTypes;
	}

	@Override
	public void notifyResource(Resource resource) {
		if (!this.resourceTypes.isEmpty() && !this.resourceTypes.contains(resource.getType()))
			return;

		if (this.resourceMap.hasElement(this.tx, resource.getType(), resource.getId())) {
			if (this.updateResources) {
				this.resourceMap.update(this.tx, resource);
			}
		} else if (this.addResources) {
			this.resourceMap.add(this.tx, resource);
		}
	}

	@Override
	public void notifyOrder(Order order) {
		if (!this.orderTypes.isEmpty() && !this.orderTypes.contains(order.getType()))
			return;

		if (this.orderMap.hasElement(this.tx, order.getType(), order.getId())) {
			if (this.updateOrders) {
				this.orderMap.update(this.tx, order);
			}
		} else if (this.addOrders) {
			this.orderMap.add(this.tx, order);
		}
	}
}
