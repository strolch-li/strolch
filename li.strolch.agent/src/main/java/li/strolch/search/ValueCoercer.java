package li.strolch.search;

/**
 * Coerce a given value to a different value
 */
public interface ValueCoercer {

	/**
	 * Coerce the given value to a different value
	 */
	Object coerce(Object value);
}
