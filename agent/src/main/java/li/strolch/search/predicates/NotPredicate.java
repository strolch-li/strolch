package li.strolch.search.predicates;

import li.strolch.search.SearchPredicate;
import li.strolch.search.ValueCoercer;

/**
 * Implements the not predicate by negating the return value of the internal {@link SearchPredicate}
 */
public class NotPredicate implements SearchPredicate {

	private final SearchPredicate predicate;

	public NotPredicate(SearchPredicate predicate) {
		this.predicate = predicate;
	}

	@Override
	public SearchPredicate coerce(ValueCoercer coercer) {
		this.predicate.coerce(coercer);
		return this;
	}

	@Override
	public boolean matches(Object left) {
		return !this.predicate.matches(left);
	}
}
