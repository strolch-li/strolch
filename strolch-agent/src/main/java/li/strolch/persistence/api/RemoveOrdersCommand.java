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

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;

import li.strolch.agent.api.OrderMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.Order;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RemoveOrdersCommand extends Command {

	private List<Order> orders = new ArrayList<>();
	private final List<Order> removed = new ArrayList<>();

	public RemoveOrdersCommand(StrolchTransaction tx) {
		super(tx);
	}

	/**
	 * @param orders
	 * 		the orders to set for removal
	 */
	public void setOrders(List<Order> orders) {
		this.orders = orders;
	}

	/**
	 * @param order
	 * 		the order to add for removal
	 */
	public void addOrder(Order order) {
		this.orders.add(order);
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotEmpty("Orders may not be empty!", this.orders);
	}

	@Override
	public void doCommand() {

		this.orders.forEach(order -> tx().lock(order));

		OrderMap orderMap = tx().getOrderMap();

		this.orders.forEach(order -> {
			if (!orderMap.hasElement(tx(), order.getType(), order.getId())) {
				String msg = "The Order {0} can not be removed as it does not exist!!";
				msg = MessageFormat.format(msg, order.getLocator());
				throw new StrolchException(msg);
			}

			orderMap.remove(tx(), order);
			this.removed.add(order);
		});
	}

	@Override
	public void undo() {
		if (tx().isRollingBack() && !this.removed.isEmpty()) {
			this.orders.forEach(order -> {
				if (tx().isVersioningEnabled())
					tx().getOrderMap().undoVersion(tx(), order);
				else
					tx().getOrderMap().add(tx(), order);
			});
		}
	}
}
