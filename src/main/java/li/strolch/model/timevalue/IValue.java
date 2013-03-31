package li.strolch.model.timevalue;

/**
 * A value object defining some basic algebraic operations. Mathematically
 * speaking {@link IValue} objects define a group with a addition operation.
 * 
 * @author Martin Smock <smock.martin@gmail.com>
 * 
 * @param <T>
 *            any object for which a (generalized) add operation can be defined.
 * 
 */
public interface IValue<T> {

	/**
	 * @return the backing value
	 */
	T getValue();

	/**
	 * @return a value with the backing value added to the argument value
	 */
	IValue<T> add(T o);

	/**
	 * @return true, if the backing values match.
	 */
	boolean matches(IValue<T> other);

	/**
	 * @return the inverse value, such that add(value.getInverse()) returns the
	 *         neutral element of the group
	 */
	IValue<T> getInverse();

	/**
	 * @return a copy of this
	 */
	IValue<T> getCopy();

}
