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

package li.strolch.report.policy;

import li.strolch.model.StrolchValueType;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicy;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.iso8601.ISO8601;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

import java.time.LocalDateTime;
import java.time.Period;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;

public abstract class ReportFilterPolicy extends StrolchPolicy {

	public ReportFilterPolicy(StrolchTransaction tx) {
		super(tx);
	}

	protected boolean negate;
	protected String filterValue;
	protected Object right;

	public boolean isNegate() {
		return this.negate;
	}

	public void setNegate(boolean negate) {
		this.negate = negate;
	}

	public void setFilterValue(String filterValue) {
		this.filterValue = filterValue;
	}

	public void init(String value) {
		if (value.startsWith("!")) {
			setNegate(true);
			setFilterValue(value.substring(1));
		} else {
			setFilterValue(value);
		}
	}

	public boolean filter(Object value) {
		DBC.PRE.assertNotNull("value required!", value);

		Object left;
		if (value instanceof ZonedDateTime) {

			if (this.right == null)
				this.right = parseFilterValueToZdt(this.filterValue);

			left = value;

		} else if (value instanceof Date) {

			if (this.right == null) {
				logger.error("DEPRECATED, use ZonedDateTime");
				this.right = parseFilterValueToDate(this.filterValue);
			}

			left = value;

		} else if (value instanceof Parameter<?> parameter) {

			if (this.right == null) {
				StrolchValueType valueType = parameter.getValueType();
				if (valueType == StrolchValueType.DATE)
					this.right = parseFilterValueToZdt(this.filterValue);
				else
					this.right = valueType.parseValue(this.filterValue);
			}

			if (value instanceof DateParameter)
				left = ((DateParameter) parameter).getValueZdt();
			else
				left = parameter.getValue();

		} else {
			if (this.right == null)
				this.right = this.filterValue;

			left = value.toString();
		}

		return filter(left, this.right, this.negate);
	}

	public boolean filter(Object value1, Object value2) {
		throw new UnsupportedOperationException("Handling multiple values not supported!");
	}

	protected Date parseFilterValueToDate(String filterValue) {
		DBC.INTERIM.assertNotEmpty("filterValue must not be empty for date comparisons!", filterValue);
		logger.error("DEPRECATED, use ZonedDateTime");

		if (!filterValue.startsWith("now"))
			return ISO8601FormatFactory.getInstance().parseDate(filterValue);

		if (filterValue.charAt(3) != '(' || filterValue.charAt(filterValue.length() - 1) != ')')
			throw new IllegalArgumentException("now() format invalid for " + filterValue);

		String periodS = filterValue.substring(4, filterValue.length() - 1);
		if (periodS.isEmpty())
			return new Date();

		Period period = Period.parse(periodS);
		LocalDateTime dateTime = LocalDateTime.now().plus(period);
		return Date.from(dateTime.atZone(ZoneId.systemDefault()).toInstant());
	}

	protected ZonedDateTime parseFilterValueToZdt(String filterValue) {
		DBC.INTERIM.assertNotEmpty("filterValue must not be empty for date comparisons!", filterValue);

		if (!filterValue.startsWith("now"))
			return ISO8601.parseToZdt(filterValue);

		if (filterValue.charAt(3) != '(' || filterValue.charAt(filterValue.length() - 1) != ')')
			throw new IllegalArgumentException("now() format invalid for " + filterValue);

		String periodS = filterValue.substring(4, filterValue.length() - 1);
		if (periodS.isEmpty())
			return ZonedDateTime.now();

		Period period = Period.parse(periodS);
		return ZonedDateTime.now().plus(period);
	}

	protected abstract boolean filter(Object left, Object right, boolean negate);

	@Override
	public void undo() {
		// do nothing
	}
}
