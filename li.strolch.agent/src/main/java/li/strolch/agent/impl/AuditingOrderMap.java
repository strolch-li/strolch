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

import java.util.HashSet;
import java.util.List;

import li.strolch.agent.api.AuditTrail;
import li.strolch.agent.api.ElementMap;
import li.strolch.agent.api.OrderMap;
import li.strolch.model.Order;
import li.strolch.model.Tags;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.visitor.OrderVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

/**
 * This is the {@link AuditTrail} for {@link Order Orders}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 * @see AuditingElementMapFacade
 */
public class AuditingOrderMap extends AuditingElementMapFacade<Order> implements OrderMap {

	public AuditingOrderMap(ElementMap<Order> elementMap, boolean observeAccessReads) {
		super(elementMap, observeAccessReads);
	}

	@Override
	protected String getElementType() {
		return Tags.ORDER;
	}

	@Override
	protected OrderMap getElementMap() {
		return (OrderMap) super.getElementMap();
	}

	@Override
	public <U> List<U> doQuery(StrolchTransaction tx, OrderQuery<U> query) {
		OrderVisitor<U> orderVisitor = query.getVisitor();
		DBC.PRE.assertNotNull("orderVisitor on query", orderVisitor);
		query.setVisitor(order -> {
			if (this.read == null)
				this.read = new HashSet<>();
			this.read.add(order);
			return order.accept(orderVisitor);
		});

		return getElementMap().doQuery(tx, query);
	}
}
