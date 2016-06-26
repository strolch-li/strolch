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
package li.strolch.runtime.query.inmemory;

import java.util.List;

import li.strolch.model.Order;
import li.strolch.model.OrderVisitor;
import li.strolch.model.query.DateSelection;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.OrderQueryVisitor;
import li.strolch.model.query.StateSelection;
import li.strolch.model.query.StrolchTypeNavigation;
import li.strolch.persistence.api.OrderDao;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class InMemoryOrderQueryVisitor extends InMemoryQueryVisitor<Order, OrderDao> implements OrderQueryVisitor {

	public InMemoryOrderQueryVisitor() {
		super();
	}

	@Override
	protected InMemoryQueryVisitor<Order, OrderDao> newInstance() {
		return new InMemoryOrderQueryVisitor();
	}

	public <U> InMemoryQuery<Order, U> visit(OrderQuery<U> orderQuery) {
		OrderVisitor<U> orderVisitor = orderQuery.getOrderVisitor();
		DBC.PRE.assertNotNull("OrderVisitor may not be null!", orderVisitor); //$NON-NLS-1$

		orderQuery.accept(this);

		Navigator<Order> navigator = getNavigator();
		if (navigator == null) {
			String msg = "Query is missing a navigation!"; //$NON-NLS-1$
			throw new QueryException(msg);
		}

		List<Selector<Order>> selectors = getSelectors();
		if (selectors.isEmpty())
			return new InMemoryQuery<>(navigator, null, orderVisitor, getComparator());

		DBC.PRE.assertTrue("Invalid query as it may only contain one selector!", selectors.size() == 1); //$NON-NLS-1$
		return new InMemoryQuery<>(navigator, selectors.get(0), orderVisitor, getComparator());
	}

	@Override
	public void visit(StrolchTypeNavigation navigation) {
		setNavigator(new OrderTypeNavigator(navigation.getType()));
	}

	@Override
	public void visit(DateSelection selection) {
		addSelector(new DateSelector(selection.getDateRange()));
	}

	@Override
	public void visit(StateSelection selection) {
		addSelector(new StateSelector(selection.getState()));
	}
}
