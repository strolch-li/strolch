package li.strolch.search.predicates;

import li.strolch.search.SearchPredicate;
import li.strolch.search.ValueCoercer;

public abstract class AbstractSearchPredicate implements SearchPredicate {

	private boolean coerced;
	protected Object right;

	public AbstractSearchPredicate(Object right) {
		this.right = right;
	}

	public AbstractSearchPredicate coerce(ValueCoercer coercer) {
		if (this.coerced)
			return this;

		this.right = coercer.coerce(this.right);
		this.coerced = true;
		return this;
	}
}
