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
import li.strolch.utils.collections.DateRange;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DateSelector implements Selector<Order> {

	private DateRange dateRange;

	public DateSelector(DateRange dateRange) {
		this.dateRange = dateRange;
	}

	/**
	 * @return the dateRange
	 */
	public DateRange getDateRange() {
		return this.dateRange;
	}

	@Override
	public boolean select(Order element) {
		return this.dateRange.contains(element.getDate());
	}
}
