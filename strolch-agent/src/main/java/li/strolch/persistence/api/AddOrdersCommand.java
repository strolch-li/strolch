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

import li.strolch.agent.api.OrderMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.Order;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AddOrdersCommand extends Command {

	private List<Order> orders = new ArrayList<>();
	private final List<Order> added = new ArrayList<>();

	public AddOrdersCommand(StrolchTransaction tx) {
		super(tx);
	}

	/**
	 * @param orders
	 * 		the orders to set for adding
	 */
	public void setOrders(List<Order> orders) {
		this.orders = orders;
	}

	/**
	 * @param resource
	 * 		the resource to add for adding
	 */
	public void addOrder(Order resource) {
		this.orders.add(resource);
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("Orders may not be null!", this.orders);
	}

	@Override
	public void doCommand() {

		this.orders.forEach(resource -> tx().lock(resource));

		OrderMap resourceMap = tx().getOrderMap();

		this.orders.forEach(resource -> {
			if (resourceMap.hasElement(tx(), resource.getType(), resource.getId())) {
				String msg = MessageFormat.format("The Order {0} already exists!", resource.getLocator());
				throw new StrolchException(msg);
			}

			resourceMap.add(tx(), resource);
			this.added.add(resource);
		});
	}

	@Override
	public void undo() {
		if (tx().isRollingBack() && !this.added.isEmpty()) {
			this.added.forEach(resource -> {
				OrderMap resourceMap = tx().getOrderMap();
				if (resourceMap.hasElement(tx(), resource.getType(), resource.getId()))
					resourceMap.remove(tx(), resource);
			});
		}
	}
}
