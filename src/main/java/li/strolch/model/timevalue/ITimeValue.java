package li.strolch.model.timevalue;

import li.strolch.model.timevalue.impl.TimeVariable;

/**
 * Interface for timed value objects to be used with the {@link TimeVariable}
 * 
 * @author Martin Smock <smock.martin@gmail.com>
 * 
 * @param <T>
 *            the backing value of the timed value object
 */
@SuppressWarnings("rawtypes")
public interface ITimeValue<T extends IValue> extends Comparable<ITimeValue<T>> {

	ITimeValue<T> setValue(final T value);

	T getValue();

	Long getTime();

	ITimeValue<T> add(final T change);

}
