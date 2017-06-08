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
import java.util.Collection;
import java.util.Iterator;
import java.util.SortedSet;
import java.util.TreeSet;

import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.ITimeVariable;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;

/**
 * @author Martin Smock <smock.martin@gmail.com>
 */
@SuppressWarnings("rawtypes")
public class TimeVariable<T extends IValue> implements ITimeVariable<T>, Serializable {

	private static final long serialVersionUID = 1L;

	public SortedSet<ITimeValue<T>> container = new TreeSet<>();

	@Override
	public ITimeValue<T> getValueAt(final Long time) {
		ITimeValue<T> tmp = null;
		for (ITimeValue<T> value : this.container) {
			if (value.getTime() <= time) {
				tmp = value;
			} else {
				break;
			}
		}
		return tmp;
	}

	@Override
	public void setValueAt(final Long time, final T targetValue) {
		ITimeValue<T> current = getValueAt(time);
		if (current != null && current.getTime().equals(time)) {
			current.setValue(targetValue);
		} else {
			this.container.add(new TimeValue<>(time, targetValue));
		}
	}

	@Override
	public SortedSet<ITimeValue<T>> getFutureValues(final Long time) {
		TimeValue<T> picker = new TimeValue<>(time, null);
		return new TreeSet<>(this.container.tailSet(picker));
	}

	@Override
	public Collection<ITimeValue<T>> getPastValues(final Long time) {
		TimeValue<T> picker = new TimeValue<>(time, null);
		return new TreeSet<>(this.container.headSet(picker));
	}

	@Override
	public SortedSet<ITimeValue<T>> getValues() {
		return new TreeSet<>(this.container);
	}

	@Override
	public void applyChange(final IValueChange<T> change, boolean compact) {

		SortedSet<ITimeValue<T>> futureValues = getFutureValues(change.getTime());
		for (ITimeValue<T> value : futureValues) {
			value.add(change.getValue());
		}

		ITimeValue<T> initialValue = getValueAt(change.getTime());
		if (initialValue == null) {
			ITimeValue<T> newValue = new TimeValue<>(change.getTime(), change.getValue());
			this.container.add(newValue);
		} else if (initialValue.getTime().longValue() < change.getTime().longValue()) {
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
}
