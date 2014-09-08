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

/**
 * {@link OrderQuery} is the user API to query {@link Order Orders} in Strolch. The {@link Navigation} is used to
 * navigate to a type of order on which any further {@link Selection Selections} will be performed. The
 * {@link OrderVisitor} is used to transform the returned object into a domain specific object (if required). This
 * mechanism allows you to query only the values of a {@link Parameter} instead of having to return all the elements and
 * then performing this transformation.
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class OrderQuery extends StrolchElementQuery<OrderQueryVisitor> {

	public OrderQuery(Navigation navigation) {
		super(navigation);
	}

	@Override
	public OrderQuery with(Selection selection) {
		super.with(selection);
		return this;
	}

	@Override
	public OrderQuery not(Selection selection) {
		super.not(selection);
		return this;
	}

	@Override
	public OrderQuery withAny() {
		super.withAny();
		return this;
	}

	public static OrderQuery query(Navigation navigation) {
		return new OrderQuery(navigation);
	}

	public static OrderQuery query(String type) {
		return new OrderQuery(new StrolchTypeNavigation(type));
	}
}
