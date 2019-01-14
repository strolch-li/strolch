package li.strolch.search;

/**
 * Defines a search expression interface to perform where clauses on an object
 */
@FunctionalInterface
public interface ValueSearchExpression<T> {

	/**
	 * See if this search expression matches the given element
	 *
	 * @param element
	 * 		the element to match
	 *
	 * @return true if the element is matched with this search expression
	 */
	boolean matches(T element);

	/**
	 * Returns a new search expression where this search expression is ORed with the given search expression
	 *
	 * @param right
	 * 		the right hand side of the search expression
	 *
	 * @return the new search expression with an internal OR of the two search expressions
	 */
	default ValueSearchExpression<T> or(ValueSearchExpression<T> right) {
		return element -> this.matches(element) || right.matches(element);
	}

	/**
	 * Returns a new search expression where this search expression is ANDed with the given search expression
	 *
	 * @param right
	 * 		the right hand side of the search expression
	 *
	 * @return the new search expression with an internal AND of the two search expressions
	 */
	default ValueSearchExpression<T> and(ValueSearchExpression<T> right) {
		return element -> this.matches(element) && right.matches(element);
	}

	/**
	 * Negates this search expression
	 *
	 * @return a new search expression where this search expression is negated
	 */
	default ValueSearchExpression<T> not() {
		return element -> !this.matches(element);
	}
}
