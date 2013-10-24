package li.strolch.model.timevalue;

import java.util.Collection;
import java.util.SortedSet;

/**
 * A timed variable storing a ordered sequence of {@link ITimeValue} objects
 * modeling a time evolution of a quantity.
 * 
 * @author Martin Smock <smock.martin@gmail.com>
 * 
 * @param <T>
 *            the backing value of the timed value object
 */
public interface ITimeVariable<T extends IValue<?>> {

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
	 * get the latest {@link ITimeValue} whose time field is less or equal to
	 * the time given
	 */
	ITimeValue<T> getValueAt(final Long time);

	/**
	 * Applies a {@link IValueChange} propagating the change to all future
	 * values starting from the time of the change.
	 * 
	 * @param change
	 *            the {@link IValueChange} to be applied
	 */
	void applyChange(final IValueChange<T> change);

	/**
	 * Get all {@link ITimeValue} objects whose time field is greater or equal
	 * to the given time
	 * 
	 * @param time
	 *            the time the sequence starts with
	 * @return the sequence of {@link ITimeValue} objects in the future
	 */
	Collection<ITimeValue<T>> getFutureValues(final Long time);

	/**
	 * Get all {@link ITimeValue} objects whose time field is strictly smaller
	 * than the given time
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
	 * removes {@link ITimeValue} objects from the sequence, where the successor
	 * matches value. I.e considering a pair of adjacent {@link ITimeValue}
	 * objects in the sequence which have the same {@link IValue}, the later one
	 * is removed, since it contains no additional information.
	 */
	void compact();

}
