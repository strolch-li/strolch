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

import static java.util.Collections.unmodifiableNavigableSet;

import java.io.Serializable;
import java.util.*;
import java.util.stream.Stream;

import li.strolch.exception.StrolchModelException;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.ITimeVariable;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;

/**
 * @author Martin Smock <smock.martin@gmail.com>
 */
@SuppressWarnings("rawtypes")
public class TimeVariable<T extends IValue> implements ITimeVariable<T>, Serializable {

	public NavigableSet<ITimeValue<T>> container = new TreeSet<>();
	private boolean readonly;

	@Override
	public int size() {
		return this.container.size();
	}

	@Override
	public ITimeValue<T> getValueAt(long time) {
		return this.container.floor(new TimeValue<>(time, null));
	}

	@Override
	public void setValueAt(long time, final T targetValue) {
		assertNotReadonly();
		ITimeValue<T> current = getValueAt(time);
		if (current != null && current.getTime().equals(time)) {
			current.setValue(targetValue);
		} else {
			this.container.add(new TimeValue<>(time, targetValue));
		}
	}

	@Override
	public NavigableSet<ITimeValue<T>> getFutureValues(long time) {
		return new TreeSet<>(this.container.tailSet(new TimeValue<>(time, null)));
	}

	@Override
	public NavigableSet<ITimeValue<T>> removeFutureValues(long time) {
		assertNotReadonly();
		SortedSet<ITimeValue<T>> values = this.container.tailSet(new TimeValue<>(time, null));
		TreeSet<ITimeValue<T>> result = new TreeSet<>(values);
		values.clear();
		return result;
	}

	@Override
	public NavigableSet<ITimeValue<T>> getPastValues(long time) {
		return new TreeSet<>(this.container.headSet(new TimeValue<>(time, null)));
	}

	@Override
	public NavigableSet<ITimeValue<T>> removePastValues(long time) {
		assertNotReadonly();
		SortedSet<ITimeValue<T>> values = this.container.headSet(new TimeValue<>(time, null));
		TreeSet<ITimeValue<T>> result = new TreeSet<>(values);
		values.clear();
		return result;
	}

	@Override
	public NavigableSet<ITimeValue<T>> getValues() {
		return unmodifiableNavigableSet(this.container);
	}

	@Override
	public Stream<ITimeValue<T>> streamValues() {
		return this.container.stream();
	}

	@Override
	public void applyChange(final IValueChange<T> change, boolean compact) {
		assertNotReadonly();

		NavigableSet<ITimeValue<T>> futureValues = getFutureValues(change.getTime());
		for (ITimeValue<T> value : futureValues) {
			value.add(change.getValue());
		}

		ITimeValue<T> initialValue = getValueAt(change.getTime());
		if (initialValue == null) {
			ITimeValue<T> newValue = new TimeValue<>(change.getTime(), change.getValue());
			this.container.add(newValue);
		} else if (initialValue.getTime() < change.getTime()) {
			ITimeValue<T> newValue = new TimeValue<>(change.getTime(), initialValue.getValue());
			newValue.add(change.getValue());
			this.container.add(newValue);
		}

		if (compact)
			compact();
	}

	@SuppressWarnings("unchecked")
	@Override
	public void compact() {
		assertNotReadonly();

		if (this.container.size() < 2) {
			return;
		}

		Iterator<ITimeValue<T>> iterator = this.container.iterator();
		ITimeValue<T> predecessor = iterator.next();

		while (iterator.hasNext()) {
			ITimeValue<T> successor = iterator.next();
			if (successor.getValue().matches(predecessor.getValue())) {
				iterator.remove();
			} else {
				predecessor = successor;
			}
		}
	}

	@Override
	public void clear() {
		assertNotReadonly();
		this.container.clear();
	}

	@Override
	public ITimeVariable<T> getCopy() {
		TimeVariable<T> clone = new TimeVariable<>();
		for (ITimeValue<T> timeValue : this.container) {
			clone.container.add(timeValue.getCopy());
		}

		return clone;
	}

	@Override
	public boolean isReadonly() {
		return this.readonly;
	}

	@Override
	public void setReadonly() {
		this.readonly = true;
	}

	protected void assertNotReadonly() {
		if (this.readonly) {
			throw new StrolchModelException("The element " + this.getClass().getSimpleName()
					+ " is currently readOnly, to modify clone first!");
		}
	}

	@Override
	public boolean equals(ITimeVariable<T> o) {
		return equals((Object) o);
	}

	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (o == null || getClass() != o.getClass())
			return false;
		@SuppressWarnings("unchecked")
		TimeVariable<T> other = (TimeVariable<T>) o;
		if (this.container.size() != other.container.size())
			return false;
		Iterator<ITimeValue<T>> thisIter = this.container.iterator();
		Iterator<ITimeValue<T>> thatIter = other.container.iterator();
		while (thisIter.hasNext()) {
			ITimeValue<T> thisNext = thisIter.next();
			ITimeValue<T> thatNext = thatIter.next();
			if (!thisNext.equals(thatNext))
				return false;
		}

		return true;
	}

	@Override
	public int hashCode() {
		return Objects.hash(this.container);
	}
}
