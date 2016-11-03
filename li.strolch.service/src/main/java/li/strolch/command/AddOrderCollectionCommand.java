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
package li.strolch.command;

import java.text.MessageFormat;
import java.util.List;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.OrderMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.Order;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AddOrderCollectionCommand extends Command {

	private List<Order> orders;
	private boolean added;

	/**
	 * @param tx
	 */
	public AddOrderCollectionCommand(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	/**
	 * @param orders
	 *            the orders to set
	 */
	public void setOrders(List<Order> orders) {
		this.orders = orders;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("Order list may not be null!", this.orders);
	}

	@Override
	public void doCommand() {

		for (Order order : this.orders) {
			tx().lock(order);
		}

		OrderMap orderMap = tx().getOrderMap();
		for (Order order : this.orders) {
			if (orderMap.hasElement(tx(), order.getType(), order.getId())) {
				String msg = MessageFormat.format("The Order {0} already exists!", order.getLocator());
				throw new StrolchException(msg);
			}
		}

		orderMap.addAll(tx(), this.orders);
		this.added = true;
	}

	@Override
	public void undo() {
		if (this.added && this.orders != null && !this.orders.isEmpty() && tx().isRollingBack()) {
			OrderMap orderMap = tx().getOrderMap();
			for (Order order : this.orders) {
				if (orderMap.hasElement(tx(), order.getType(), order.getId())) {
					orderMap.remove(tx(), order);
				}
			}
		}
	}
}
