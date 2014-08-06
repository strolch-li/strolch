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
import li.strolch.model.visitor.NoStrategyOrderVisitor;

/**
 * {@link OrderQuery} is the user API to query {@link Order Orders} in Strolch. The {@link Navigation} is used to
 * navigate to a type of order on which any further {@link Selection Selections} will be performed. The
 * {@link OrderVisitor} is used to transform the returned object into a domain specific object (if required). This
 * mechanism allows you to query only the values of a {@link Parameter} instead of having to return all the elements and
 * then performing this transformation.
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class OrderQuery<U> extends StrolchQuery<OrderQueryVisitor> {

	private OrderVisitor<U> elementVisitor;

	public OrderQuery(Navigation navigation, OrderVisitor<U> elementVisitor) {
		super(navigation);
		this.elementVisitor = elementVisitor;
	}

	/**
	 * @return the elementVisitor
	 */
	public OrderVisitor<U> getElementVisitor() {
		return this.elementVisitor;
	}

	/**
	 * Returns an instance of {@link OrderQuery} where the visitor used is the {@link NoStrategyOrderVisitor} thus
	 * returning the actual Order, i.e. no transformation is performed
	 * 
	 * @param navigation
	 * @return
	 */
	public static OrderQuery<Order> orderQuery(Navigation navigation) {
		return new OrderQuery<Order>(navigation, new NoStrategyOrderVisitor());
	}

	/**
	 * Returns an instance of {@link OrderQuery} where the visitor used is the {@link NoStrategyOrderVisitor} thus
	 * returning the actual Order, i.e. no transformation is performed
	 * 
	 * @param type
	 *            the type of Order to navigate to
	 * @return
	 */
	public static OrderQuery<Order> orderQuery(String type) {
		return new OrderQuery<Order>(new StrolchTypeNavigation(type), new NoStrategyOrderVisitor());
	}

	/**
	 * Returns an instance of {@link OrderQuery} using the given {@link OrderVisitor} thus performing the given
	 * transformation
	 * 
	 * @param type
	 *            the type of Order to navigate to
	 * @param orderVisitor
	 *            the visitor to use for transformation
	 * @return
	 */
	public static <U> OrderQuery<U> orderQuery(String type, OrderVisitor<U> orderVisitor) {
		return new OrderQuery<U>(new StrolchTypeNavigation(type), orderVisitor);
	}
}
