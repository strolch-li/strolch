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

import li.strolch.model.StrolchValueType;
import li.strolch.model.visitor.StrolchElementVisitor;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class LongParameter extends AbstractParameter<Long> {

	protected Long value;

	/**
	 * Empty constructor
	 */
	public LongParameter() {
		//
	}

	/**
	 * Default constructor
	 *
	 * @param id
	 * 		the id
	 * @param name
	 * 		the name
	 * @param value
	 * 		the value
	 */
	public LongParameter(String id, String name, Long value) {
		super(id, name);
		setValue(value);
	}

	@Override
	public String getValueAsString() {
		return this.value.toString();
	}

	@SuppressWarnings("unchecked")
	@Override
	public Long getValue() {
		return this.value;
	}

	@Override
	public void setValue(Long value) {
		assertNotReadonly();
		validateValue(value);
		this.value = value;
	}

	@Override
	public void setValueFrom(Parameter<Long> parameter) {
		assertNotReadonly();
		this.value = parameter.getValue();
	}

	/**
	 * Sets the value to 0L
	 *
	 * @see Parameter#clear()
	 */
	@Override
	public void clear() {
		assertNotReadonly();
		this.value = 0L;
	}

	/**
	 * @return true if the value == 0L
	 */
	@Override
	public boolean isEmpty() {
		return this.value == 0L;
	}

	@Override
	public boolean isEqualTo(Parameter<Long> otherValue) {
		LongParameter other = (LongParameter) otherValue;
		return this.value.equals(other.value);
	}

	@Override
	public boolean isEqualTo(Long otherValue) {
		return this.value.equals(otherValue);
	}

	public void add(long value) {
		assertNotReadonly();
		this.value += value;
	}

	public void subtract(long value) {
		assertNotReadonly();
		this.value -= value;
	}

	public void multiply(long value) {
		assertNotReadonly();
		this.value *= value;
	}

	public void divide(long value) {
		assertNotReadonly();
		this.value /= value;
	}

	public void increment() {
		assertNotReadonly();
		this.value++;
	}

	public void decrement() {
		assertNotReadonly();
		this.value--;
	}

	@Override
	public void setValueFromString(String valueAsString) {
		setValue(parseFromString(valueAsString));
	}

	@Override
	public String getType() {
		return StrolchValueType.LONG.getType();
	}

	@Override
	public StrolchValueType getValueType() {
		return StrolchValueType.LONG;
	}

	@Override
	public LongParameter getClone() {
		LongParameter clone = new LongParameter();
		super.fillClone(clone);
		clone.value = this.value;
		return clone;
	}

	@Override
	public <U> U accept(StrolchElementVisitor<U> visitor) {
		return visitor.visitLongParam(this);
	}

	public static Long parseFromString(String valueS) {
		return Long.valueOf(valueS);
	}

	@Override
	public int compareTo(Parameter<?> o) {
		DBC.PRE.assertEquals("Not same Parameter types!", this.getType(), o.getType());
		return this.getValue().compareTo(((LongParameter) o).getValue());
	}

}
