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
package li.strolch.runtime.query.inmemory;

import li.strolch.model.Order;
import li.strolch.runtime.agent.ElementMap;
import li.strolch.runtime.agent.OrderMap;
import li.strolch.runtime.component.ComponentContainer;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class OrderTypeNavigator extends StrolchTypeNavigator<Order> {

	private ComponentContainer container;

	/**
	 * @param type
	 */
	public OrderTypeNavigator(String type, ComponentContainer container) {
		super(type);
		this.container = container;
	}

	@Override
	protected ElementMap<Order> getElementMap() {
		return this.container.getComponent(OrderMap.class);
	}
}
