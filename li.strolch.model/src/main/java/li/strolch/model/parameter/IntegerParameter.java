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
public class IntegerParameter extends AbstractParameter<Integer> {

	private Integer value = Integer.MAX_VALUE;

	/**
	 * Empty constructor
	 */
	public IntegerParameter() {
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
	public IntegerParameter(String id, String name, Integer value) {
		super(id, name);
		setValue(value);
	}

	@Override
	public String getType() {
		return StrolchValueType.INTEGER.getType();
	}

	@Override
	public StrolchValueType getValueType() {
		return StrolchValueType.INTEGER;
	}

	@Override
	public String getValueAsString() {
		return Integer.toString(this.value);
	}

	@SuppressWarnings("unchecked")
	@Override
	public Integer getValue() {
		return this.value;
	}

	@Override
	public void setValue(Integer value) {
		assertNotReadonly();
		validateValue(value);
		this.value = value;
	}

	@Override
	public void setValueFrom(Parameter<Integer> parameter) {
		assertNotReadonly();
		this.value = parameter.getValue();
	}

	/**
	 * Sets the value to 0
	 *
	 * @see Parameter#clear()
	 */
	@Override
	public void clear() {
		assertNotReadonly();
		this.value = 0;
	}

	/**
	 * @return true if the value == 0
	 */
	@Override
	public boolean isEmpty() {
		return this.value == 0;
	}

	@Override
	public boolean isEqualTo(Parameter<Integer> otherValue) {
		return this.value.equals(otherValue.getValue());
	}

	@Override
	public boolean isEqualTo(Integer otherValue) {
		return this.value.equals(otherValue);
	}

	public void add(int value) {
		assertNotReadonly();
		this.value += value;
	}

	public void subtract(int value) {
		assertNotReadonly();
		this.value -= value;
	}

	public void multiply(int value) {
		assertNotReadonly();
		this.value *= value;
	}

	public void divide(int value) {
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
	public IntegerParameter getClone() {
		IntegerParameter clone = new IntegerParameter();

		super.fillClone(clone);

		clone.setValue(this.value);

		return clone;
	}

	@Override
	public <U> U accept(StrolchElementVisitor<U> visitor) {
		return visitor.visitIntegerParam(this);
	}

	public static Integer parseFromString(String valueS) {
		return Integer.valueOf(valueS);
	}

	@Override
	public int compareTo(Parameter<?> o) {
		DBC.PRE.assertEquals("Not same Parameter types!", this.getType(), o.getType());
		return this.getValue().compareTo(((IntegerParameter) o).getValue());
	}
}
