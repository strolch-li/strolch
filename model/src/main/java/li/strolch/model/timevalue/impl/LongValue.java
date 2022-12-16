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
 * {@link IValue} implementation to work with Long valued {@link ITimeValue} objects
 *
 * @author Martin Smock <smock.martin@gmail.com>
 */
public class LongValue implements IValue<Long>, Serializable {

	public static final LongValue NEUTRAL = new LongValue(0L);

	private Long value;

	public LongValue(Long value) {
		this.value = value;
	}

	public LongValue(String valueAsString) throws NumberFormatException {
		this.value = Long.parseLong(valueAsString);
	}

	@Override
	public String getType() {
		return StrolchValueType.LONG.getType();
	}

	@Override
	public LongValue add(Long o) {
		this.value += o;
		return this;
	}

	@Override
	public boolean matches(IValue<Long> other) {
		return this.value.equals(other.getValue());
	}

	@Override
	public Long getValue() {
		return this.value;
	}

	@Override
	public LongValue getInverse() {
		return new LongValue(-getValue());
	}

	@Override
	public String getValueAsString() {
		return this.value.toString();
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("LongValue [value=");
		sb.append(this.value);
		sb.append("]");
		return sb.toString();
	}

	@Override
	public LongValue getCopy() {
		return new LongValue(this.value);
	}

	@Override
	public int compareTo(IValue<Long> o) {
		return Long.compare(this.value, o.getValue());
	}

	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (o == null || getClass() != o.getClass())
			return false;
		LongValue longValue = (LongValue) o;
		return Objects.equals(value, longValue.value);
	}

	@Override
	public int hashCode() {
		return Objects.hash(value);
	}
}
