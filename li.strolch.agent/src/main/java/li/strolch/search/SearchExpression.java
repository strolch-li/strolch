package li.strolch.search;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.activity.Activity;

/**
 * Defines a search expression interface to perform where clauses on {@link StrolchRootElement}
 *
 * @param <T>
 */
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
	default SearchExpression<T> or(SearchExpression<T> right) {
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
	default SearchExpression<T> and(SearchExpression<T> right) {
		return element -> this.matches(element) && right.matches(element);
	}

	/**
	 * Negates this search expression
	 *
	 * @return a new search expression where this search expression is negated
	 */
	default SearchExpression<T> not() {
		return element -> !this.matches(element);
	}

	/**
	 * Map this search expression to a {@link Resource} search expression
	 *
	 * @return a new search expression for Resource elements
	 */
	default SearchExpression<Resource> asResource() {
		@SuppressWarnings("unchecked")
		SearchExpression<Resource> exp = element -> this.matches((T) element);
		return exp;
	}

	/**
	 * Map this search expression to a {@link Order} search expression
	 *
	 * @return a new search expression for Order elements
	 */
	default SearchExpression<Order> asOrder() {
		@SuppressWarnings("unchecked")
		SearchExpression<Order> exp = element -> this.matches((T) element);
		return exp;
	}

	/**
	 * Map this search expression to a {@link Activity} search expression
	 *
	 * @return a new search expression for Activity elements
	 */
	default SearchExpression<Activity> asActivity() {
		@SuppressWarnings("unchecked")
		SearchExpression<Activity> exp = element -> this.matches((T) element);
		return exp;
	}
}
