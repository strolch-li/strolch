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

import li.strolch.model.Order;
import li.strolch.model.query.DateSelection;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.OrderQueryVisitor;
import li.strolch.model.query.StateSelection;
import li.strolch.model.query.StrolchTypeNavigation;
import li.strolch.runtime.agent.ComponentContainer;

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
