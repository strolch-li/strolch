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

import java.util.Date;

import li.strolch.utils.collections.DateRange;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DateSelection extends OrderSelection {

	private DateRange dateRange;

	public DateSelection() {
		this.dateRange = new DateRange();
	}

	public DateSelection from(Date from, boolean inclusive) {
		this.dateRange.from(from, inclusive);
		return this;
	}

	public DateSelection to(Date to, boolean inclusive) {
		this.dateRange.to(to, inclusive);
		return this;
	}

	public Date getFromDate() {
		return this.dateRange.getFromDate();
	}

	public Date getToDate() {
		return this.dateRange.getToDate();
	}

	public boolean isFromBounded() {
		return this.dateRange.isFromBounded();
	}

	public boolean isToBounded() {
		return this.dateRange.isToBounded();
	}

	public boolean isUnbounded() {
		return this.dateRange.isUnbounded();
	}

	public boolean isBounded() {
		return this.dateRange.isBounded();
	}

	public boolean contains(Date date) {
		return this.dateRange.contains(date);
	}

	public DateRange getDateRange() {
		return this.dateRange;
	}

	@Override
	public void accept(OrderSelectionVisitor visitor) {
		visitor.visit(this);
	}
}
