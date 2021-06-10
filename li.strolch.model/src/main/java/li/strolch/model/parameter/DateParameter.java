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

import static li.strolch.utils.iso8601.ISO8601.EMPTY_VALUE_ZONED_DATE;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;

import li.strolch.model.StrolchValueType;
import li.strolch.model.visitor.StrolchElementVisitor;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.iso8601.ISO8601;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DateParameter extends AbstractParameter<Date> {

	private ZonedDateTime value;

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
	public DateParameter(String id, String name, LocalDateTime value) {
		super(id, name);
		setValueFromLocalDateTime(value);
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
	public DateParameter(String id, String name, ZonedDateTime value) {
		super(id, name);
		setValueFromZonedDateTime(value);
	}

	@Override
	public String getValueAsString() {
		return ISO8601.toString(this.value);
	}

	@SuppressWarnings("unchecked")
	@Override
	public Date getValue() {
		return Date.from(this.value.toInstant());
	}

	public ZonedDateTime getValueZdt() {
		return this.value;
	}

	public LocalDateTime getValueLdt() {
		return this.value.toLocalDateTime();
	}

	@Override
	public void setValue(Date value) {
		assertNotReadonly();
		validateValue(value);
		this.value = ZonedDateTime.ofInstant(value.toInstant(), ZoneId.systemDefault());
	}

	@Override
	public void setValueFrom(Parameter<Date> parameter) {
		assertNotReadonly();
		DateParameter other = (DateParameter) parameter;
		this.value = other.getValueZdt();
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
		this.value = EMPTY_VALUE_ZONED_DATE;
	}

	@Override
	public boolean isEmpty() {
		return this.value.equals(EMPTY_VALUE_ZONED_DATE);
	}

	@Override
	public boolean isEqualTo(Parameter<Date> otherValue) {
		DateParameter other = (DateParameter) otherValue;
		return this.value.equals(other.value);
	}

	@Override
	public boolean isEqualTo(Date otherValue) {
		return getValue().equals(otherValue);
	}

	public boolean isEqualTo(LocalDateTime otherValue) {
		return this.value.toLocalDateTime().equals(otherValue);
	}

	public boolean isEqualTo(ZonedDateTime otherValue) {
		return this.value.equals(otherValue);
	}

	public void setValueFromLocalDateTime(LocalDateTime localDateTime) {
		this.value = localDateTime.atZone(ZoneId.systemDefault());
	}

	public void setValueFromZonedDateTime(ZonedDateTime zonedDateTime) {
		this.value = zonedDateTime;
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

		clone.setValueFromZonedDateTime(this.value);

		return clone;
	}

	@Override
	public <U> U accept(StrolchElementVisitor<U> visitor) {
		return visitor.visitDateParam(this);
	}

	public static Date parseFromString(String valueS) {
		return ISO8601.parseToDate(valueS);
	}

	@Override
	public int compareTo(Parameter<?> o) {
		DBC.PRE.assertEquals("Not same Parameter types!", this.getType(), o.getType());
		return this.getValue().compareTo(((DateParameter) o).getValue());
	}
}
