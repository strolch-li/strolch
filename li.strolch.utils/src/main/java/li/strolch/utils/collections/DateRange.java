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
package li.strolch.utils.collections;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;

import li.strolch.utils.dbc.DBC;
import li.strolch.utils.iso8601.ISO8601;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class DateRange {

	private boolean fromInclusive;
	private boolean toInclusive;
	private ZonedDateTime fromDate;
	private ZonedDateTime toDate;

	public DateRange from(LocalDate from, boolean inclusive) {
		this.fromDate = ZonedDateTime.of(from.atStartOfDay(), ZoneId.systemDefault());
		this.fromInclusive = inclusive;
		validate();
		return this;
	}

	public DateRange to(LocalDate to, boolean inclusive) {
		this.toDate = ZonedDateTime.of(to.atStartOfDay(), ZoneId.systemDefault());
		this.toInclusive = inclusive;
		validate();
		return this;
	}

	public DateRange from(LocalDateTime from, boolean inclusive) {
		this.fromDate = ZonedDateTime.of(from, ZoneId.systemDefault());
		this.fromInclusive = inclusive;
		validate();
		return this;
	}

	public DateRange to(LocalDateTime to, boolean inclusive) {
		this.toDate = ZonedDateTime.of(to, ZoneId.systemDefault());
		this.toInclusive = inclusive;
		validate();
		return this;
	}

	public DateRange from(ZonedDateTime from, boolean inclusive) {
		this.fromDate = from;
		this.fromInclusive = inclusive;
		validate();
		return this;
	}

	public DateRange to(ZonedDateTime to, boolean inclusive) {
		this.toDate = to;
		this.toInclusive = inclusive;
		validate();
		return this;
	}

	public DateRange from(Date from, boolean inclusive) {
		this.fromDate = ZonedDateTime.ofInstant(from.toInstant(), ZoneId.systemDefault());
		this.fromInclusive = inclusive;
		validate();
		return this;
	}

	public DateRange to(Date to, boolean inclusive) {
		this.toDate = ZonedDateTime.ofInstant(to.toInstant(), ZoneId.systemDefault());
		this.toInclusive = inclusive;
		validate();
		return this;
	}

	private void validate() {
		if (this.toDate != null && this.fromDate != null)
			DBC.INTERIM.assertTrue("From must be before to!", this.toDate.compareTo(this.fromDate) >= 0); //$NON-NLS-1$
	}

	public boolean isFromInclusive() {
		return this.fromInclusive;
	}

	public boolean isToInclusive() {
		return this.toInclusive;
	}

	public ZonedDateTime getFromDateZdt() {
		return this.fromDate;
	}

	public ZonedDateTime getToDateZdt() {
		return this.toDate;
	}

	public LocalDateTime getFromDateLdt() {
		return this.fromDate.toLocalDateTime();
	}

	public LocalDateTime getToDateLdt() {
		return this.toDate.toLocalDateTime();
	}

	public Date getFromDate() {
		return Date.from(this.fromDate.toInstant());
	}

	public Date getToDate() {
		return Date.from(this.toDate.toInstant());
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
		return contains(ZonedDateTime.ofInstant(date.toInstant(), ZoneId.systemDefault()));
	}

	public boolean contains(LocalDateTime date) {
		return contains(ZonedDateTime.of(date, ZoneId.systemDefault()));
	}

	public boolean contains(ZonedDateTime date) {
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

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(this.fromDate == null ? "-" : ISO8601.toString(this.fromDate));
		sb.append((this.fromInclusive ? " (inc)" : " (exc)"));
		sb.append(" - ");
		sb.append(this.toDate == null ? "-" : ISO8601.toString(this.toDate));
		sb.append((this.toInclusive ? " (inc)" : " (exc)"));
		return sb.toString();
	}
}
