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

/**
 * {@link IValue} implementation to work with Boolean valued {@link ITimeValue} objects
 *
 * @author Martin Smock <smock.martin@gmail.com>
 */
public class BooleanValue implements IValue<Boolean>, Serializable {

	public static final BooleanValue NEUTRAL = new BooleanValue(false);

	private Boolean value;

	public BooleanValue(Boolean value) {
		this.value = value;
	}

	public BooleanValue(String valueAsString) throws NumberFormatException {
		this.value = Boolean.parseBoolean(valueAsString);
	}

	@Override
	public String getType() {
		return StrolchValueType.BOOLEAN.getType();
	}

	@Override
	public BooleanValue add(Boolean o) {
		this.value = o;
		return this;
	}

	@Override
	public boolean matches(IValue<Boolean> other) {
		return this.value.equals(other.getValue());
	}

	@Override
	public Boolean getValue() {
		return this.value;
	}

	@Override
	public BooleanValue getInverse() {
		return new BooleanValue(!getValue());
	}

	@Override
	public String getValueAsString() {
		return this.value.toString();
	}

	@Override
	public String toString() {
		String sb = "BooleanValue [value=" + this.value + "]";
		return sb;
	}

	@Override
	public BooleanValue getCopy() {
		return new BooleanValue(this.value);
	}

	@Override
	public int compareTo(IValue<Boolean> o) {
		return Boolean.compare(this.value, o.getValue());
	}

	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (o == null || getClass() != o.getClass())
			return false;
		BooleanValue that = (BooleanValue) o;
		return Objects.equals(value, that.value);
	}

	@Override
	public int hashCode() {
		return Objects.hash(value);
	}
}
