/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
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
import li.strolch.model.activity.Activity;

import java.util.stream.Stream;

import static li.strolch.model.StrolchModelConstants.PolicyConstants.PARAM_ORDER;

/**
 * Performs a search for {@link Activity} elements
 */
public class ActivitySearch extends StrolchSearch<Activity, ActivitySearchResult> {

	private SearchNavigator<Activity> navigator;

	@Override
	protected SearchNavigator<Activity> getNavigator() {
		return this.navigator;
	}

	@Override
	public ActivitySearch types(String... types) {
		this.navigator = tx -> {
			Stream<Activity> cachedStream = tx.streamCachedActivities(types);
			Stream<Activity> stream = tx
					.streamActivities(types)
					.filter(e -> !tx.isActivityCached(e.getType(), e.getId()));
			return Stream.concat(cachedStream, stream);
		};
		return this;
	}

	public ActivitySearch forOrder(Order order) {
		types(order.getType());
		where(relation(PARAM_ORDER).isEqualTo(order.getId()));
		return this;
	}

	@Override
	public ActivitySearch where(SearchExpression<Activity> expression) {
		super.where(expression);
		return this;
	}

	@Override
	public ActivitySearch internal() {
		super.internal();
		return this;
	}

	@Override
	protected ActivitySearchResult evaluateResult(Stream<Activity> stream) {
		return new ActivitySearchResult(stream);
	}
}
