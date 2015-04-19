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
package li.strolch.model.xml;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import li.strolch.model.Order;
import li.strolch.model.Resource;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class SimpleStrolchElementListener implements StrolchElementListener {

	private List<Resource> resources;
	private List<Order> orders;

	@Override
	public void notifyResource(Resource resource) {
		if (this.resources == null) {
			this.resources = new ArrayList<>();
		}
		this.resources.add(resource);
	}

	@Override
	public void notifyOrder(Order order) {
		if (this.orders == null) {
			this.orders = new ArrayList<>();
		}
		this.orders.add(order);
	}

	/**
	 * @return the resources
	 */
	public List<Resource> getResources() {
		if (this.resources == null)
			return Collections.emptyList();
		return this.resources;
	}

	/**
	 * @return the orders
	 */
	public List<Order> getOrders() {
		if (this.orders == null)
			return Collections.emptyList();
		return this.orders;
	}
}
