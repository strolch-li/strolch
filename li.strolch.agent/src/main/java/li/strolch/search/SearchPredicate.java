package li.strolch.search;

import li.strolch.search.predicates.NotPredicate;

public interface SearchPredicate {

	boolean matches(Object left);

	SearchPredicate coerce(ValueCoercer coercer);

	default SearchPredicate not() {
		return new NotPredicate(this);
	}
}
