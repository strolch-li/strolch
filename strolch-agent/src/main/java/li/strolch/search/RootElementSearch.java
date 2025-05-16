/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.activity.Activity;

import java.util.stream.Stream;

/**
 * Performs a search for any kind of root element, allowing to mix {@link Resource}, {@link Order} and {@link Activity}
 * in the result
 */
public class RootElementSearch extends StrolchSearch<StrolchRootElement, RootElementSearchResult<StrolchRootElement>> {

	private SearchNavigator<StrolchRootElement> navigator;

	@Override
	protected SearchNavigator<StrolchRootElement> getNavigator() {
		return this.navigator;
	}

	@Override
	public RootElementSearch types(String... types) {
		this.navigator = tx -> {

			Stream<Resource> resources = tx.streamResources(types);
			Stream<Activity> activities = tx.streamActivities(types);
			Stream<Order> orders = tx.streamOrders(types);

			return Stream.concat(resources, Stream.concat(activities, orders));
		};

		return this;
	}

	@Override
	protected RootElementSearchResult<StrolchRootElement> evaluateResult(Stream<StrolchRootElement> stream) {
		return new RootElementSearchResult<>(stream);
	}
}
