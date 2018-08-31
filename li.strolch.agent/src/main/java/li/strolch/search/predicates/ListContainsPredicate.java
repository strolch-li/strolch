package li.strolch.search.predicates;

import li.strolch.utils.ObjectHelper;

/**
 * Implements the contains predicate, delegating to {@link ObjectHelper#isIn(Object, Object, boolean)}
 */
public class ListContainsPredicate extends AbstractSearchPredicate {
	private boolean ignoreCase;

	public ListContainsPredicate(Object right, boolean ignoreCase) {
		super(right);
		this.ignoreCase = ignoreCase;
	}

	@Override
	public boolean matches(Object left) {
		return ObjectHelper.isIn(this.right, left, this.ignoreCase);
	}
}
