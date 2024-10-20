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

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RemoveOrderCommand extends Command {

	private Order order;
	private boolean removed;

	/**
	 * @param tx
	 */
	public RemoveOrderCommand(StrolchTransaction tx) {
		super(tx);
	}

	/**
	 * @param order
	 *            the order to set
	 */
	public void setOrder(Order order) {
		this.order = order;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("Order may not be null!", this.order);
	}

	@Override
	public void doCommand() {

		tx().lock(this.order);

		OrderMap orderMap = tx().getOrderMap();
		if (!orderMap.hasElement(tx(), this.order.getType(), this.order.getId())) {
			String msg = "The Order {0} can not be removed as it does not exist!";
			msg = MessageFormat.format(msg, this.order.getLocator());
			throw new StrolchException(msg);
		}

		orderMap.remove(tx(), this.order);
		this.removed = true;
	}

	@Override
	public void undo() {
		if (this.order != null && tx().isRollingBack() && this.removed) {
			if (tx().isVersioningEnabled())
				tx().getOrderMap().undoVersion(tx(), this.order);
			else
				tx().getOrderMap().add(tx(), this.order);
		}
	}
}
