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

import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.IValue;

/**
 * {@link IValue} implementation to work with Double valued {@link ITimeValue} objects
 * 
 * @author Martin Smock <smock.martin@gmail.com>
 */
public class DoubleValue implements IValue<Double>, Serializable {

	private static final long serialVersionUID = 1L;

	public static final DoubleValue NEUTRAL = new DoubleValue(0.0d);

	private Double value;

	public DoubleValue(Double value) {
		this.value = value;
	}

	public DoubleValue(double value) {
		this.value = Double.valueOf(value);
	}

	public DoubleValue(Integer value) {
		this.value = this.value.doubleValue();
	}

	public DoubleValue(int value) {
		this.value = Integer.valueOf(value).doubleValue();
	}

	public DoubleValue(String valueAsString) throws NumberFormatException {
		this.value = Double.parseDouble(valueAsString);
	}

	@Override
	public DoubleValue add(Double o) {
		this.value += o;
		return this;
	}

	@Override
	public Double getValue() {
		return this.value;
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("DoubleValue [value=");
		sb.append(this.value);
		sb.append("]");
		return sb.toString();
	}

	@Override
	public boolean matches(IValue<Double> other) {
		return this.value.equals(other.getValue());
	}

	@Override
	public DoubleValue getInverse() {
		return new DoubleValue(-getValue());
	}

	@Override
	public DoubleValue getCopy(){
		return new DoubleValue(this.value); 
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.value == null) ? 0 : this.value.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		DoubleValue other = (DoubleValue) obj;
		if (this.value == null) {
			if (other.value != null)
				return false;
		} else if (!this.value.equals(other.value))
			return false;
		return true;
	}
	

}
