package li.strolch.search.predicates;

import li.strolch.utils.ObjectHelper;

/**
 * Implements the contains predicate, delegating to {@link ObjectHelper#contains(Object, Object, boolean, boolean)}
 */
public class ContainsPredicate extends AbstractSearchPredicate {
	private final boolean ignoreCase;
	private final boolean matchAny;

	public ContainsPredicate(Object right, boolean ignoreCase, boolean matchAny) {
		super(right);
		this.ignoreCase = ignoreCase;
		this.matchAny = matchAny;
	}

	@Override
	public boolean matches(Object left) {
		return ObjectHelper.contains(left, this.right, this.ignoreCase, !this.matchAny);
	}
}
