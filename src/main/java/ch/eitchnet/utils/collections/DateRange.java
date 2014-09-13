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
package ch.eitchnet.utils.collections;

import java.util.Date;

import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DateRange {

	private boolean fromInclusive;
	private boolean toInclusive;
	private Date fromDate;
	private Date toDate;

	public DateRange from(Date from, boolean inclusive) {
		this.fromDate = from;
		this.fromInclusive = inclusive;
		validate();
		return this;
	}

	public DateRange to(Date to, boolean inclusive) {
		this.toDate = to;
		this.toInclusive = inclusive;
		validate();
		return this;
	}

	private void validate() {
		if (this.toDate != null && this.fromDate != null)
			DBC.INTERIM.assertTrue("From must be before to!", this.toDate.compareTo(this.fromDate) >= 0); //$NON-NLS-1$
	}

	/**
	 * @return from date
	 */
	public Date getFromDate() {
		return this.fromDate;
	}

	/**
	 * @return to date
	 */
	public Date getToDate() {
		return this.toDate;
	}

	/**
	 * @return true if from is set
	 */
	public boolean isFromBounded() {
		return this.fromDate != null;
	}

	/**
	 * @return true if to is set
	 */
	public boolean isToBounded() {
		return this.toDate != null;
	}

	/**
	 * @return true if both from and to are null
	 */
	public boolean isUnbounded() {
		return this.fromDate == null && this.toDate == null;
	}

	/**
	 * @return true if both from and to date are set
	 */
	public boolean isBounded() {
		return this.fromDate != null && this.toDate != null;
	}

	/**
	 * @return true if both from and to date are set and they are both equal
	 */
	public boolean isDate() {
		return isBounded() && this.fromDate.equals(this.toDate);
	}

	public boolean contains(Date date) {
		DBC.PRE.assertNotNull("Date must be given!", date); //$NON-NLS-1$
		if (this.fromDate == null && this.toDate == null)
			return true;

		boolean fromContains = true;
		boolean toContains = true;

		if (this.toDate != null) {
			int compare = this.toDate.compareTo(date);
			if (this.toInclusive)
				toContains = compare >= 0;
			else
				toContains = compare > 0;
		}

		if (this.fromDate != null) {
			int compare = this.fromDate.compareTo(date);
			if (this.fromInclusive)
				fromContains = compare <= 0;
			else
				fromContains = compare < 0;
		}
		return toContains && fromContains;
	}
}
