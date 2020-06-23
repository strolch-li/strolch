package li.strolch.search.predicates;

import li.strolch.utils.ObjectHelper;

/**
 * Implements the is empty predicate, delegating to {@link ObjectHelper#equals(Object, Object, boolean)}
 */
public class IsEmptyPredicate extends AbstractSearchPredicate {

	public IsEmptyPredicate() {
		super(null);
	}

	@Override
	public boolean matches(Object left) {
		return ObjectHelper.isEmpty(left);
	}
}
