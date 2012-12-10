package li.strolch.model.timevalue;

/**
 * Interface for operators to be used to change the values of {@link ITimeValue}
 * in a {@link ITimeVariable}.
 * 
 * @author martin_smock
 */
@SuppressWarnings("rawtypes")
public interface IValueChange<T extends IValue> {

	/**
	 * @return the time this change has to be applied
	 */
	Long getTime();

	/**
	 * @return the value of the change
	 */
	T getValue();

	/**
	 * @return the inverse neutralizing a change. Very useful to undo changes
	 *         applied.
	 */
	IValueChange<T> getInverse();

}
