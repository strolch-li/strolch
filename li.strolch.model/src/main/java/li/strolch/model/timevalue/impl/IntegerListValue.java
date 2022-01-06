/*
 * Copyright 2022 Robert von Burg <eitch@eitchnet.ch>
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
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import li.strolch.model.StrolchValueType;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.IValue;
import li.strolch.utils.helper.StringHelper;

/**
 * {@link IValue} implementation to work with Integer valued {@link ITimeValue} objects
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class IntegerListValue implements IValue<List<Integer>>, Serializable {

	public static final IntegerListValue NEUTRAL = new IntegerListValue(0);

	private final List<Integer> value;

	public IntegerListValue(Integer value) {
		this.value = Collections.singletonList(value);
	}

	public IntegerListValue(int... values) {
		List<Integer> value = new ArrayList<>();
		for (Integer aInteger : values) {
			value.add(aInteger);
		}
		this.value = value;
	}

	public IntegerListValue(List<Integer> values) {
		this.value = new ArrayList<>(values);
	}

	public IntegerListValue(String valueAsString) throws NumberFormatException {
		List<Integer> value = new ArrayList<>();
		String[] values = valueAsString.split(",");
		for (String s : values) {
			value.add(Integer.parseInt(s.trim()));
		}
		this.value = value;
	}

	@Override
	public String getType() {
		return StrolchValueType.INTEGER_LIST.getType();
	}

	@Override
	public IntegerListValue add(List<Integer> o) {
		this.value.addAll(o);
		return this;
	}

	@Override
	public List<Integer> getValue() {
		return this.value;
	}

	@Override
	public String getValueAsString() {
		if (this.value.isEmpty()) {
			return StringHelper.EMPTY;
		}

		StringBuilder sb = new StringBuilder();
		Iterator<Integer> iter = this.value.iterator();
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
		sb.append("IntegerListValue [value=");
		sb.append(this.value);
		sb.append("]");
		return sb.toString();
	}

	@Override
	public boolean matches(IValue<List<Integer>> other) {
		return this.value.equals(other.getValue());
	}

	@Override
	public IntegerListValue getInverse() {
		return new IntegerListValue(this.value);
	}

	@Override
	public IntegerListValue getCopy() {
		return new IntegerListValue(this.value);
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
		IntegerListValue other = (IntegerListValue) obj;
		if (this.value == null) {
			if (other.value != null) {
				return false;
			}
		} else if (!this.value.equals(other.value)) {
			return false;
		}
		return true;
	}

	@Override
	public int compareTo(IValue<List<Integer>> o) {
		List<Integer> otherValues = o.getValue();
		if (this.value.equals(otherValues))
			return 0;
		return Integer.compare(this.value.size(), otherValues.size());
	}
}
