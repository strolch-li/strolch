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

import java.util.Comparator;
import java.util.stream.Stream;

public class OrderSearchResult extends RootElementSearchResult<Order> {
	public OrderSearchResult(Stream<Order> stream) {
		super(stream);
	}

	/**
	 * Appends a comparator to the stream of elements to compare by date
	 *
	 * @return this for chaining
	 */
	public OrderSearchResult orderByDate() {
		return orderByDate(false);
	}

	/**
	 * Appends a comparator to the stream of elements to compare by date
	 *
	 * @param reversed flag to reverse the comparison
	 *
	 * @return this for chaining
	 */
	public OrderSearchResult orderByDate(boolean reversed) {
		Comparator<Order> comparator = Comparator.comparing(Order::getDate);
		if (reversed)
			comparator = comparator.reversed();
		this.stream = this.stream.sorted(comparator);
		return this;
	}
}
