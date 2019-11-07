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
 * @author Martin Smock <smock.martin@gmail.com>
 */
@SuppressWarnings("rawtypes")
public class TimeValue<T extends IValue> implements ITimeValue<T>, Serializable {

	protected final long time;
	protected T value;

	/**
	 * @param time
	 * 		the time for this value
	 * @param value
	 * 		the actual value for this value
	 */
	public TimeValue(final long time, final T value) {
		this.time = time;
		this.value = value;
	}

	@Override
	@SuppressWarnings("unchecked")
	public T getValue() {
		return this.value == null ? null : (T) this.value.getCopy();
	}

	@Override
	public Long getTime() {
		return this.time;
	}

	@Override
	public ITimeValue<T> setValue(final T value) {
		this.value = value;
		return this;
	}

	@SuppressWarnings("unchecked")
	@Override
	public ITimeValue<T> add(final T change) {
		this.value.add(change.getValue());
		return this;
	}

	@SuppressWarnings("unchecked")
	@Override
	public int compareTo(final ITimeValue<T> arg0) {
		int i = getTime().compareTo(arg0.getTime());
		if (i != 0)
			return i;
		if (this.value == null && arg0.getValue() == null)
			return 0;
		if (this.value == null)
			return -1;
		if (arg0.getValue() == null)
			return 1;
		return getValue().compareTo(arg0.getValue());
	}

	@SuppressWarnings("unchecked")
	@Override
	public ITimeValue<T> getCopy() {
		return new TimeValue<>(this.time, (T) this.value.getCopy());
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("TimeValue [time=");
		sb.append(this.time);
		sb.append(", value=");
		sb.append(this.value);
		sb.append("]");
		return sb.toString();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + Long.hashCode(this.time);
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
		@SuppressWarnings("unchecked")
		TimeValue<T> other = (TimeValue<T>) obj;
		if (this.time != other.time) {
			return false;
		}
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
