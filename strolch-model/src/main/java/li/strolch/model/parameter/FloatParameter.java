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

import java.util.Objects;

import li.strolch.model.StrolchValueType;
import li.strolch.model.visitor.StrolchElementVisitor;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.MathHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class FloatParameter extends AbstractParameter<Double> {

	private Double value = Double.MAX_VALUE;

	/**
	 * Empty constructor
	 */
	public FloatParameter() {
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
	public FloatParameter(String id, String name, Double value) {
		super(id, name);

		setValue(value);
	}

	@Override
	public String getValueAsString() {
		return MathHelper.toPrecisionString(this.value, 8);
	}

	@SuppressWarnings("unchecked")
	@Override
	public Double getValue() {
		return MathHelper.toPrecision(this.value, 8);
	}

	@Override
	public void setValue(Double value) {
		assertNotReadonly();
		validateValue(value);
		this.value = value;
	}

	public void setValue(int value) {
		assertNotReadonly();
		this.value = (double) value;
	}

	@Override
	public void setValueFrom(Parameter<Double> parameter) {
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
		this.value = 0.0D;
	}

	/**
	 * @return true if the value == 0.0D
	 */
	@Override
	public boolean isEmpty() {
		return this.value == 0.0D;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null || getClass() != obj.getClass())
			return false;
		FloatParameter o = (FloatParameter) obj;
		return this.parent == o.parent && this.id.equals(o.id);
	}

	@Override
	public int hashCode() {
		return Objects.hash(parent, id);
	}

	@Override
	public boolean isEqualTo(Parameter<Double> otherValue) {
		FloatParameter other = (FloatParameter) otherValue;
		return this.value.equals(other.value);
	}

	@Override
	public boolean isEqualTo(Double otherValue) {
		return this.value.equals(otherValue);
	}

	public void add(double value) {
		assertNotReadonly();
		this.value += value;
	}

	public void subtract(double value) {
		assertNotReadonly();
		this.value -= value;
	}

	public void multiply(double value) {
		assertNotReadonly();
		this.value *= value;
	}

	public void divide(double value) {
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
		return StrolchValueType.FLOAT.getType();
	}

	@Override
	public StrolchValueType getValueType() {
		return StrolchValueType.FLOAT;
	}

	@Override
	public FloatParameter getClone() {
		FloatParameter clone = new FloatParameter();
		super.fillClone(clone);
		clone.value = this.value;
		return clone;
	}

	@Override
	public <U> U accept(StrolchElementVisitor<U> visitor) {
		return visitor.visitFloatParam(this);
	}

	public static Double parseFromString(String valueS) {
		return Double.valueOf(valueS);
	}

	@Override
	public int compareTo(Parameter<?> o) {
		DBC.PRE.assertEquals("Not same Parameter types!", this.getType(), o.getType());
		return this.getValue().compareTo(((FloatParameter) o).getValue());
	}
}
