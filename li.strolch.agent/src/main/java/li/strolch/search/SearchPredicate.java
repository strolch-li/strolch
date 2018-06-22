package li.strolch.search;

import li.strolch.search.predicates.NotPredicate;

/**
 * Define the search predicate, i.e. how the where clause is evaluated, or the operator with the right hand side of the
 * where clause
 */
public interface SearchPredicate {

	/**
	 * Returns true if this predicate matches the given left hand side of the where clause
	 *
	 * @param left
	 * 		the left side to match
	 *
	 * @return true if the predicate matches
	 */
	boolean matches(Object left);

	/**
	 * Coerces the internal right handle side of this predicate using the given coercer. This is required to handle
	 * situations where values are not compatible, i.e. Date object and date string
	 *
	 * @param coercer
	 * 		the coercer to be applied to the right hand side
	 *
	 * @return the new search predicate with the coerced right hand side
	 */
	SearchPredicate coerce(ValueCoercer coercer);

	/**
	 * Negates this predicated
	 *
	 * @return a new predicate where this predicate is negated
	 */
	default SearchPredicate not() {
		return new NotPredicate(this);
	}
}
