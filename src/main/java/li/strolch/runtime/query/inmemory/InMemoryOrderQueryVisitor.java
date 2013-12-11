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
import li.strolch.model.query.DateSelection;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.OrderQueryVisitor;
import li.strolch.model.query.StateSelection;
import li.strolch.model.query.StrolchTypeNavigation;
import li.strolch.runtime.component.ComponentContainer;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class InMemoryOrderQueryVisitor extends InMemoryQueryVisitor<Order> implements OrderQueryVisitor {

	public InMemoryOrderQueryVisitor(ComponentContainer container) {
		super(container);
	}

	@Override
	protected InMemoryQueryVisitor<Order> newInstance() {
		return new InMemoryOrderQueryVisitor(this.container);
	}

	public InMemoryQuery<Order> visit(OrderQuery orderQuery) {
		orderQuery.accept(this);

		if (this.navigator == null) {
			String msg = "Query is missing a navigation!"; //$NON-NLS-1$
			throw new QueryException(msg);
		}

		return new InMemoryQuery<>(this.navigator, this.selectors);
	}

	@Override
	public void visit(StrolchTypeNavigation navigation) {
		this.navigator = new OrderTypeNavigator(navigation.getType(), this.container);
	}

	@Override
	public void visit(DateSelection selection) {
		this.selectors.add(new DateSelector(selection.getDate()));
	}

	@Override
	public void visit(StateSelection selection) {
		this.selectors.add(new StateSelector(selection.getState()));
	}
}
