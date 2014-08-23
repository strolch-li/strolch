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
package li.strolch.model.audit;

import java.util.Date;

import ch.eitchnet.utils.collections.DateRange;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DateRangeSelection extends AuditSelection {

	private DateRange dateRange;

	public DateRangeSelection(AuditQuery query) {
		super(query);
	}

	public DateRange from(Date from, boolean inclusive) {
		return this.dateRange.from(from, inclusive);
	}

	public DateRange to(Date to, boolean inclusive) {
		return this.dateRange.to(to, inclusive);
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

	@Override
	public void accept(AuditQueryVisitor visitor) {
		visitor.visit(this);
	}
}
