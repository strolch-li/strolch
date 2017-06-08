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
package li.strolch.model.timevalue;

import java.util.Collection;
import java.util.SortedSet;

/**
 * A timed variable storing a ordered sequence of {@link ITimeValue} objects modeling a time evolution of a quantity.
 *
 * @author Martin Smock <smock.martin@gmail.com>
 *
 * @param <T>
 *            the backing value of the timed value object
 */
@SuppressWarnings("rawtypes")
public interface ITimeVariable<T extends IValue> {

	/**
	 * set the value at a point in time to a given time value object
	 *
	 * @param time
	 *            the time to set the {@link IValue}
	 * @param value
	 *            the {@link IValue} to set
	 */
	void setValueAt(final Long time, final T value);

	/**
	 * get the latest {@link ITimeValue} whose time field is less or equal to the time given
	 */
	ITimeValue<T> getValueAt(final Long time);

	/**
	 * Applies a {@link IValueChange} propagating the change to all future values starting from the time of the change.
	 *
	 * @param change
	 *            the {@link IValueChange} to be applied
	 * @param compact
	 *            if set to true, then the values are compacted, otherwiss not
	 */
	void applyChange(final IValueChange<T> change, boolean compact);

	/**
	 * Get all {@link ITimeValue} objects whose time field is greater or equal to the given time
	 *
	 * @param time
	 *            the time the sequence starts with
	 * @return the sequence of {@link ITimeValue} objects in the future
	 */
	Collection<ITimeValue<T>> getFutureValues(final Long time);

	/**
	 * Get all {@link ITimeValue} objects whose time field is strictly smaller than the given time
	 *
	 * @param time
	 *            the time the sequence starts with
	 * @return the sequence of {@link ITimeValue} objects in the future
	 */
	Collection<ITimeValue<T>> getPastValues(final Long time);

	/**
	 * Get all {@link ITimeValue} objects
	 *
	 * @return a defensive copy of the {@link ITimeValue}s
	 */
	SortedSet<ITimeValue<T>> getValues();

	/**
	 * removes {@link ITimeValue} objects from the sequence, where the successor matches value. I.e considering a pair
	 * of adjacent {@link ITimeValue} objects in the sequence which have the same {@link IValue}, the later one is
	 * removed, since it contains no additional information.
	 */
	void compact();

	/**
	 * Clears all values on this time variable
	 */
	void clear();

	/**
	 * @return a copy of this time variable
	 */
	ITimeVariable<T> getCopy();
}
