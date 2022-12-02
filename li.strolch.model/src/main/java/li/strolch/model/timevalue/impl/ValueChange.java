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

import static li.strolch.utils.helper.StringHelper.trimOrEmpty;

import java.io.Serializable;
import java.util.Objects;

import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;

/**
 * @author Martin Smock <smock.martin@gmail.com>
 */
@SuppressWarnings("rawtypes")
public class ValueChange<T extends IValue> implements IValueChange<T>, Serializable {

	protected Long time;
	protected T value;
	protected String stateId;
	private boolean readonly;

	/**
	 * @param time
	 * 		the time the change applies
	 * @param value
	 * 		the value to be applied
	 */
	public ValueChange(final Long time, final T value) {
		this.time = time;
		this.value = value;
	}

	/**
	 * @param time
	 * 		the time the change applies
	 * @param value
	 * 		the value to be applied
	 * @param stateId
	 * 		the id of the state the change applies to
	 */
	public ValueChange(final Long time, final T value, final String stateId) {
		this.time = time;
		this.value = value;
		this.stateId = trimOrEmpty(stateId);
	}

	@Override
	public Long getTime() {
		return this.time;
	}

	@Override
	public void setTime(Long time) {
		this.time = time;
	}

	@Override
	@SuppressWarnings("unchecked")
	public T getValue() {
		return (T) this.value.getCopy();
	}

	@Override
	public void setValue(T value) {
		this.value = value;
	}

	@Override
	@SuppressWarnings("unchecked")
	public IValueChange<T> getInverse() {
		return new ValueChange(this.time, this.value.getInverse());
	}

	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (o == null || getClass() != o.getClass())
			return false;
		ValueChange<?> that = (ValueChange<?>) o;
		return Objects.equals(time, that.time) && Objects.equals(value, that.value);
	}

	@Override
	public int hashCode() {
		return Objects.hash(time, value);
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("ValueChange [time=");
		sb.append(this.time);
		sb.append(", value=");
		sb.append(this.value);
		sb.append(", stateId=");
		sb.append(this.stateId);
		sb.append("]");
		return sb.toString();
	}

	@Override
	public String getStateId() {
		return this.stateId;
	}

	@Override
	public void setStateId(String id) {
		this.stateId = id;
	}

	@SuppressWarnings("unchecked")
	@Override
	public IValueChange<T> getClone() {
		return new ValueChange(this.time, this.value, this.stateId);
	}

	@Override
	public boolean isReadOnly() {
		return this.readonly;
	}

	@Override
	public void setReadOnly() {
		this.readonly = true;
	}

	protected void assertNotReadonly() {
		if (this.readonly) {
			throw new IllegalStateException("The element " + getClass().getSimpleName() + " for stateId " + this.stateId
					+ " is currently readOnly, to modify clone first!");
		}
	}
}
