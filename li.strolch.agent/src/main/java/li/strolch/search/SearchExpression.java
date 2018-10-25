package li.strolch.search;

import li.strolch.model.StrolchRootElement;

/**
 * Defines a search expression interface to perform where clauses on {@link StrolchRootElement}
 */
@FunctionalInterface
public interface SearchExpression<T extends StrolchRootElement> {

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
	@SuppressWarnings("unchecked")
	default <U extends StrolchRootElement> SearchExpression<U> or(SearchExpression<T> right) {
		return element -> this.matches((T) element) || right.matches((T) element);
	}

	/**
	 * Returns a new search expression where this search expression is ANDed with the given search expression
	 *
	 * @param right
	 * 		the right hand side of the search expression
	 *
	 * @return the new search expression with an internal AND of the two search expressions
	 */
	@SuppressWarnings("unchecked")
	default <U extends StrolchRootElement> SearchExpression<U> and(SearchExpression<T> right) {
		return element -> this.matches((T) element) && right.matches((T) element);
	}

	/**
	 * Negates this search expression
	 *
	 * @return a new search expression where this search expression is negated
	 */
	@SuppressWarnings("unchecked")
	default <U extends StrolchRootElement> SearchExpression<U> not() {
		return element -> !this.matches((T) element);
	}
}
