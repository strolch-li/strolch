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

import java.util.NavigableSet;
import java.util.stream.Stream;

/**
 * A timed variable storing a ordered sequence of {@link ITimeValue} objects modeling a time evolution of a quantity.
 *
 * @param <T>
 * 		the backing value of the timed value object
 *
 * @author Martin Smock <smock.martin@gmail.com>
 */
@SuppressWarnings("rawtypes")
public interface ITimeVariable<T extends IValue> {

	/**
	 * set the value at a point in time to a given time value object
	 *
	 * @param time
	 * 		the time to set the {@link IValue}
	 * @param value
	 * 		the {@link IValue} to set
	 */
	void setValueAt(long time, final T value);

	/**
	 * get the latest {@link ITimeValue} whose time field is less or equal to the time given
	 */
	ITimeValue<T> getValueAt(long time);

	/**
	 * Applies a {@link IValueChange} propagating the change to all future values starting from the time of the change.
	 *
	 * @param change
	 * 		the {@link IValueChange} to be applied
	 * @param compact
	 * 		if set to true, then the values are compacted, otherwiss not
	 */
	void applyChange(final IValueChange<T> change, boolean compact);

	/**
	 * Get all {@link ITimeValue} objects whose time field is greater or equal to the given time
	 *
	 * @param time
	 * 		the time the sequence starts with
	 *
	 * <b>Note:</b> The returned result is unmodifiable
	 *
	 * @return the sequence of {@link ITimeValue} objects in the future
	 */
	NavigableSet<ITimeValue<T>> getFutureValues(long time);

	/**
	 * Removes all {@link ITimeValue} objects whose time field is greater or equal to the given time
	 *
	 * @param time
	 * 		the time the sequence starts with
	 *
	 * @return the sequence of {@link ITimeValue} objects removed
	 */
	NavigableSet<ITimeValue<T>> removeFutureValues(long time);

	/**
	 * Get all {@link ITimeValue} objects whose time field is strictly smaller than the given time
	 *
	 * @param time
	 * 		the time the sequence starts with
	 *
	 * <b>Note:</b> The returned result is unmodifiable
	 *
	 * @return the sequence of {@link ITimeValue} objects in the future
	 */
	NavigableSet<ITimeValue<T>> getPastValues(long time);

	/**
	 * Remove all {@link ITimeValue} objects whose time field is strictly smaller than the given time
	 *
	 * @param time
	 * 		the time the sequence starts with
	 *
	 * @return the sequence of {@link ITimeValue} objects removed
	 */
	NavigableSet<ITimeValue<T>> removePastValues(long time);

	/**
	 * Returns all {@link ITimeValue} objects in an unmodifiable {@link NavigableSet}
	 *
	 * <b>Note:</b> The returned result is unmodifiable
	 *
	 * @return a defensive copy of the {@link ITimeValue}s
	 */
	NavigableSet<ITimeValue<T>> getValues();

	/**
	 * Returns a {@link Stream} over all {@link ITimeValue} objects
	 *
	 * @return a stream of all the values
	 */
	Stream<ITimeValue<T>> streamValues();

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

	/**
	 * Returns true if this element is read only, in which case modifications will throw an exception. To modify it,
	 * call <code>getClone()</code> on the parent
	 *
	 * @return true if this element is read only
	 */
	boolean isReadonly();

	/**
	 * Sets this element to readOnly, so that it may not be modified. To modify it, call <code>getClone()</code> on the
	 * parent
	 */
	void setReadonly();
}
