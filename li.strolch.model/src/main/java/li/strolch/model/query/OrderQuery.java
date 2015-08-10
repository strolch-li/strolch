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
package li.strolch.model.query;

import li.strolch.model.Order;
import li.strolch.model.OrderVisitor;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.query.ordering.StrolchQueryOrdering;
import li.strolch.model.visitor.NoStrategyOrderVisitor;
import ch.eitchnet.utils.dbc.DBC;

/**
 * <p>
 * {@link OrderQuery} is the user API to query {@link Order Orders} in Strolch. The {@link Navigation} is used to
 * navigate to a type of order on which any further {@link Selection Selections} will be performed. The
 * {@link OrderVisitor} is used to transform the returned object into a domain specific object (if required). This
 * mechanism allows you to query only the values of a {@link Parameter} instead of having to return all the elements and
 * then performing this transformation.
 * </p>
 * 
 * <p>
 * The {@link OrderVisitor} is intended for situations where the query result should not be {@link Order} but some other
 * object type. For instance in a restful API, the result might have to be mapped to a POJO, thus using this method can
 * perform the mapping step for you
 * </p>
 * 
 * @param <U>
 *            defines the return type of this query. Depending on the user {@link OrderVisitor} this query can return an
 *            {@link Order}, or any type of object to which the visitor mapped the order
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class OrderQuery<U> extends StrolchElementQuery<OrderQueryVisitor> {

	protected OrderVisitor<U> orderVisitor;
	protected StrolchQueryOrdering ordering;

	public OrderQuery(Navigation navigation) {
		super(navigation);
	}

	public OrderVisitor<U> getOrderVisitor() {
		return this.orderVisitor;
	}

	public OrderQuery<U> setOrderVisitor(OrderVisitor<U> orderVisitor) {
		DBC.PRE.assertNotNull("orderVisitor", orderVisitor);
		this.orderVisitor = orderVisitor;
		return this;
	}

	public StrolchQueryOrdering getOrdering() {
		return this.ordering;
	}

	public OrderQuery<U> setOrdering(StrolchQueryOrdering ordering) {
		this.ordering = ordering;
		return this;
	}

	@Override
	public OrderQuery<U> with(Selection selection) {
		super.with(selection);
		return this;
	}

	@Override
	public OrderQuery<U> not(Selection selection) {
		super.not(selection);
		return this;
	}

	@Override
	public OrderQuery<U> withAny() {
		super.withAny();
		return this;
	}

	@Override
	public void accept(OrderQueryVisitor visitor) {
		DBC.PRE.assertNotNull("orderVisitor", this.orderVisitor);
		super.accept(visitor);
		if (this.ordering != null)
			this.ordering.accept(visitor);
	}

	public static OrderQuery<Order> query(String type) {
		return new OrderQuery<Order>(new StrolchTypeNavigation(type)).setOrderVisitor(new NoStrategyOrderVisitor());
	}

	public static OrderQuery<Order> query(String type, StrolchQueryOrdering ordering) {
		return new OrderQuery<Order>(new StrolchTypeNavigation(type)).setOrderVisitor(new NoStrategyOrderVisitor())
				.setOrdering(ordering);
	}

	public static <U> OrderQuery<U> query(String type, OrderVisitor<U> orderVisitor) {
		return new OrderQuery<U>(new StrolchTypeNavigation(type)).setOrderVisitor(orderVisitor);
	}

	public static <U> OrderQuery<U> query(String type, OrderVisitor<U> orderVisitor, StrolchQueryOrdering ordering) {
		return new OrderQuery<U>(new StrolchTypeNavigation(type)).setOrdering(ordering).setOrderVisitor(orderVisitor);
	}
}
