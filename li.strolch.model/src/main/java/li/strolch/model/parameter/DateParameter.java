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
package li.strolch.model.parameter;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;

import li.strolch.model.StrolchValueType;
import li.strolch.model.visitor.StrolchElementVisitor;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DateParameter extends AbstractParameter<Date> {

	private static final long serialVersionUID = 0L;
	private static final Date EMPTY_VALUE = ISO8601FormatFactory.getInstance().getDateFormat().parse("-");

	private Date value;

	/**
	 * Empty constructor
	 */
	public DateParameter() {
		//
	}

	/**
	 * Default Constructor
	 *
	 * @param id
	 * 		the id
	 * @param name
	 * 		the name
	 * @param value
	 * 		the value
	 */
	public DateParameter(String id, String name, Date value) {
		super(id, name);
		setValue(value);
	}

	@Override
	public String getValueAsString() {
		return ISO8601FormatFactory.getInstance().formatDate(this.value);
	}

	@SuppressWarnings("unchecked")
	@Override
	public Date getValue() {
		return this.value;
	}

	@Override
	public void setValue(Date value) {
		assertNotReadonly();
		validateValue(value);
		this.value = value;
	}

	@Override
	public void setValue(Parameter<Date> parameter) {
		assertNotReadonly();
		this.value = parameter.getValue();
	}

	@Override
	public void setValueFromString(String valueAsString) {
		setValue(parseFromString(valueAsString));
	}

	/**
	 * Sets the value to 1970-01-01 (unix time 0)
	 *
	 * @see Parameter#clear()
	 */
	@Override
	public void clear() {
		assertNotReadonly();
		this.value = EMPTY_VALUE;
	}

	@Override
	public boolean isEmpty() {
		return this.value.equals(EMPTY_VALUE);
	}

	@Override
	public boolean isEqualTo(Parameter<Date> otherValue) {
		return this.value.equals(otherValue.getValue());
	}

	@Override
	public boolean isEqualTo(Date otherValue) {
		return this.value.equals(otherValue);
	}

	public boolean isEqualTo(LocalDateTime otherValue) {
		return this.value.equals(Date.from(otherValue.atZone(ZoneId.systemDefault()).toInstant()));
	}

	public boolean isEqualTo(ZonedDateTime otherValue) {
		return this.value.equals(Date.from(otherValue.toInstant()));
	}

	public ZonedDateTime toZonedDateTime() {
		return ZonedDateTime.ofInstant(this.value.toInstant(), ZoneId.systemDefault());
	}

	public LocalDateTime toLocalDateTime() {
		return LocalDateTime.ofInstant(this.value.toInstant(), ZoneId.systemDefault());
	}

	public void setValueFromLocalDateTime(LocalDateTime localDateTime) {
		this.value = Date.from(localDateTime.atZone(ZoneId.systemDefault()).toInstant());
	}

	public void setValueFromZonedDateTime(ZonedDateTime zonedDateTime) {
		this.value = Date.from(zonedDateTime.toInstant());
	}

	@Override
	public String getType() {
		return StrolchValueType.DATE.getType();
	}

	@Override
	public StrolchValueType getValueType() {
		return StrolchValueType.DATE;
	}

	@Override
	public DateParameter getClone() {
		DateParameter clone = new DateParameter();

		super.fillClone(clone);

		clone.setValue(this.value);

		return clone;
	}

	@Override
	public <U> U accept(StrolchElementVisitor<U> visitor) {
		return visitor.visitDateParam(this);
	}

	public static Date parseFromString(String valueS) {
		return ISO8601FormatFactory.getInstance().getDateFormat().parse(valueS);
	}

	@Override
	public int compareTo(Parameter<?> o) {
		DBC.PRE.assertEquals("Not same Parameter types!", this.getType(), o.getType());
		return this.getValue().compareTo(((DateParameter) o).getValue());
	}
}
