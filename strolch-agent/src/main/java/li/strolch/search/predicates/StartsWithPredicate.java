package li.strolch.search.predicates;

import li.strolch.utils.ObjectHelper;

/**
 * Implements the startsWith predicate, delegating to {@link ObjectHelper#startsWith(Object, Object, boolean)}
 */
public class StartsWithPredicate extends AbstractSearchPredicate {
	private final boolean ignoreCase;

	public StartsWithPredicate(Object right, boolean ignoreCase) {
		super(right);
		this.ignoreCase = ignoreCase;
	}

	@Override
	public boolean matches(Object left) {
		return ObjectHelper.startsWith(left, this.right, this.ignoreCase);
	}
}