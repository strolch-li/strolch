package li.strolch.search.predicates;

import li.strolch.utils.ObjectHelper;

/**
 * Implements the contains predicate, delegating to {@link ObjectHelper#contains(Object, Object, boolean)}
 */
public class ContainsPredicate extends AbstractSearchPredicate {
	private final boolean ignoreCase;

	public ContainsPredicate(Object right, boolean ignoreCase) {
		super(right);
		this.ignoreCase = ignoreCase;
	}

	@Override
	public boolean matches(Object left) {
		return ObjectHelper.contains(left, this.right, this.ignoreCase);
	}
}
