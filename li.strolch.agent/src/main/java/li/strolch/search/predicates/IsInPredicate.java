package li.strolch.search.predicates;

import li.strolch.utils.ObjectHelper;

public class IsInPredicate extends AbstractSearchPredicate {
	private final boolean ignoreCase;

	public IsInPredicate(Object right, boolean ignoreCase) {
		super(right);
		this.ignoreCase = ignoreCase;
	}

	@Override
	public boolean matches(Object left) {
		return ObjectHelper.isIn(left, this.right, this.ignoreCase);
	}
}
