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
package li.strolch.runtime.agent;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.xml.StrolchElementListener;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class InMemoryElementListener implements StrolchElementListener {

	private ResourceMap resourceMap;
	private OrderMap orderMap;

	public InMemoryElementListener(ResourceMap resourceMap, OrderMap orderMap) {
		this.resourceMap = resourceMap;
		this.orderMap = orderMap;
	}

	@Override
	public void notifyResource(Resource resource) {
		this.resourceMap.add(resource);
	}

	@Override
	public void notifyOrder(Order order) {
		this.orderMap.add(order);
	}

}
