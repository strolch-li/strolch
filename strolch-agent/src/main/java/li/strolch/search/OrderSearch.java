/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.search;

import li.strolch.model.Order;
import li.strolch.model.State;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;

/**
 * Performs a search of {@link Order} elements
 */
public class OrderSearch extends StrolchSearch<Order, OrderSearchResult> {

	private SearchNavigator<Order> navigator;

	@Override
	protected SearchNavigator<Order> getNavigator() {
		return this.navigator;
	}

	@Override
	public OrderSearch types(String... types) {
		this.navigator = tx -> {
			Stream<Order> cachedStream = tx.streamCachedOrders(types);
			Stream<Order> stream = tx.streamOrders(types).filter(e -> !tx.isOrderCached(e.getType(), e.getId()));
			return Stream.concat(cachedStream, stream);
		};
		return this;
	}

	@Override
	public OrderSearch where(SearchExpression<Order> expression) {
		super.where(expression);
		return this;
	}

	public OrderSearch stateIsIn(String states) {
		if (states == null)
			return this;
		String trim = states.trim();
		if (trim.isEmpty())
			return this;

		List<State> stateList = Arrays.stream(trim.split(",")) //
				.map(e -> State.parse(e.trim())) //
				.collect(toList());
		super.where(state().isIn(stateList));
		return this;
	}

	public OrderSearch stateIsIn(State... states) {
		super.where(state().isIn(states));
		return this;
	}

	public OrderSearch withState(State state) {
		super.where(state().isEqualTo(state));
		return this;
	}

	@Override
	public OrderSearch internal() {
		super.internal();
		return this;
	}

	@Override
	protected OrderSearchResult evaluateResult(Stream<Order> stream) {
		return new OrderSearchResult(stream);
	}
}
