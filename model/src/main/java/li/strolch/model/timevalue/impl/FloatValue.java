/*
 * Copyright 2013 Martin Smock <smock.martin@gmail.com>
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
package li.strolch.model.timevalue.impl;

import java.io.Serializable;
import java.util.Objects;

import li.strolch.model.StrolchValueType;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.IValue;
import li.strolch.utils.helper.MathHelper;

/**
 * {@link IValue} implementation to work with Double valued {@link ITimeValue} objects
 *
 * @author Martin Smock <smock.martin@gmail.com>
 */
public class FloatValue implements IValue<Double>, Serializable {

	public static final FloatValue NEUTRAL = new FloatValue(0.0d);

	private Double value;

	public FloatValue(Double value) {
		this.value = value;
	}

	public FloatValue(Integer value) {
		this.value = value.doubleValue();
	}

	public FloatValue(Long value) {
		this.value = value.doubleValue();
	}

	public FloatValue(String valueAsString) throws NumberFormatException {
		this.value = Double.parseDouble(valueAsString);
	}

	@Override
	public String getType() {
		return StrolchValueType.FLOAT.getType();
	}

	@Override
	public FloatValue add(Double o) {
		this.value += o;
		return this;
	}

	@Override
	public Double getValue() {
		return MathHelper.toPrecision(this.value, 8);
	}

	@Override
	public String getValueAsString() {
		return MathHelper.toPrecisionString(this.value, 8);
	}

	@Override
	public String toString() {
		return "DoubleValue [value=" + this.value + "]";
	}

	@Override
	public boolean matches(IValue<Double> other) {
		return this.value.equals(other.getValue());
	}

	@Override
	public FloatValue getInverse() {
		return new FloatValue(-getValue());
	}

	@Override
	public FloatValue getCopy() {
		return new FloatValue(this.value);
	}

	@Override
	public int compareTo(IValue<Double> o) {
		return Double.compare(this.value, o.getValue());
	}

	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (o == null || getClass() != o.getClass())
			return false;
		FloatValue that = (FloatValue) o;
		return Objects.equals(value, that.value);
	}

	@Override
	public int hashCode() {
		return Objects.hash(value);
	}
}
