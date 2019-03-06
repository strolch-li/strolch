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

import static li.strolch.model.parameter.ListParameter.VALUE_SEPARATOR2;

import java.io.Serializable;
import java.util.*;

import li.strolch.model.StrolchValueType;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.IValue;
import li.strolch.utils.helper.StringHelper;

/**
 * {@link IValue} implementation to work with Double valued {@link ITimeValue} objects
 *
 * @author Martin Smock <smock.martin@gmail.com>
 */
public class FloatListValue implements IValue<List<Double>>, Serializable {

	public static final FloatListValue NEUTRAL = new FloatListValue(0.0d);

	private List<Double> value;

	public FloatListValue(Double value) {
		this.value = Collections.singletonList(value);
	}

	public FloatListValue(double... values) {
		List<Double> value = new ArrayList<>();
		for (Double aDouble : values) {
			value.add(aDouble);
		}
		this.value = value;
	}

	public FloatListValue(List<Double> values) {
		this.value = new ArrayList<>(values);
	}

	public FloatListValue(String valueAsString) throws NumberFormatException {
		List<Double> value = new ArrayList<>();
		String[] values = valueAsString.split(",");
		for (String s : values) {
			value.add(Double.parseDouble(s.trim()));
		}
		this.value = value;
	}

	@Override
	public String getType() {
		return StrolchValueType.FLOAT_LIST.getType();
	}

	@Override
	public FloatListValue add(List<Double> o) {
		this.value.addAll(o);
		return this;
	}

	@Override
	public List<Double> getValue() {
		return this.value;
	}

	@Override
	public String getValueAsString() {
		if (this.value.isEmpty()) {
			return StringHelper.EMPTY;
		}

		StringBuilder sb = new StringBuilder();
		Iterator<Double> iter = this.value.iterator();
		while (iter.hasNext()) {

			sb.append(iter.next());

			if (iter.hasNext()) {
				sb.append(VALUE_SEPARATOR2);
				sb.append(" ");
			}
		}

		return sb.toString();
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("FloatListValue [value=");
		sb.append(this.value);
		sb.append("]");
		return sb.toString();
	}

	@Override
	public boolean matches(IValue<List<Double>> other) {
		return this.value.equals(other.getValue());
	}

	@Override
	public FloatListValue getInverse() {
		return new FloatListValue(this.value);
	}

	@Override
	public FloatListValue getCopy() {
		return new FloatListValue(this.value);
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
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		FloatListValue other = (FloatListValue) obj;
		if (this.value == null) {
			if (other.value != null) {
				return false;
			}
		} else if (!this.value.equals(other.value)) {
			return false;
		}
		return true;
	}

}
