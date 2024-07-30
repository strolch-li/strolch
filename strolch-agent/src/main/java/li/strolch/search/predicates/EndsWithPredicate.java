package li.strolch.search.predicates;

import li.strolch.utils.ObjectHelper;

/**
 * Implements the endsWith predicate, delegating to {@link ObjectHelper#endsWith(Object, Object, boolean)}
 */
public class EndsWithPredicate extends AbstractSearchPredicate {
	private final boolean ignoreCase;

	public EndsWithPredicate(Object right, boolean ignoreCase) {
		super(right);
		this.ignoreCase = ignoreCase;
	}

	@Override
	public boolean matches(Object left) {
		return ObjectHelper.endsWith(left, this.right, this.ignoreCase);
	}
}