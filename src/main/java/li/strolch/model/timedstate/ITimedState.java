package li.strolch.model.timedstate;

import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.ITimeVariable;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;

/**
 * A time based state characterized by a {@link IValue} object implementation.
 * 
 * @author martin_smock
 * 
 * @param <T>
 *            IValue implementation representing the state at a given time
 */
@SuppressWarnings("rawtypes")
public interface ITimedState<T extends IValue> {

	/**
	 * @return the new {@link ITimeValue} matching the value in the future
	 */
	ITimeValue<T> getNextMatch(final Long time, T value);

	/**
	 * @return the new {@link ITimeValue} matching the value in the future
	 */
	ITimeValue<T> getPreviousMatch(final Long time, T value);

	/**
	 * @param change
	 *            the state change to be applied
	 */
	void applyChange(final IValueChange<T> change);

	/**
	 * @return the state at the given time
	 */
	ITimeValue<T> getStateAt(final Long time);

	/**
	 * @return get the states evolution in time
	 */
	ITimeVariable<T> getTimeEvolution();

}