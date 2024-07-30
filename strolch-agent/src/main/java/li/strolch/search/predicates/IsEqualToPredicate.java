package li.strolch.search.predicates;

import li.strolch.utils.ObjectHelper;

/**
 * Implements the equals predicate, delegating to {@link ObjectHelper#equals(Object, Object, boolean)}
 */
public class IsEqualToPredicate extends AbstractSearchPredicate {
	private final boolean ignoreCase;

	public IsEqualToPredicate(Object right, boolean ignoreCase) {
		super(right);
		this.ignoreCase = ignoreCase;
	}

	@Override
	public boolean matches(Object left) {
		return ObjectHelper.equals(left, this.right, this.ignoreCase);
	}
}
