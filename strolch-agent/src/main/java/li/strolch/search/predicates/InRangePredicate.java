package li.strolch.search.predicates;

import java.time.ZonedDateTime;
import java.util.Date;

import li.strolch.search.SearchPredicate;
import li.strolch.search.ValueCoercer;
import li.strolch.utils.collections.DateRange;

/**
 * <p>Implements the date in range predicate.</p>
 *
 * <b>Note:</b> Can only be used with {@link Date} or {@link ZonedDateTime} objects
 */
public class InRangePredicate implements SearchPredicate {
	private final DateRange range;

	public InRangePredicate(DateRange range) {
		this.range = range;
	}

	@Override
	public boolean matches(Object left) {
		if (left instanceof Date)
			return this.range.contains((Date) left);
		else if (left instanceof ZonedDateTime)
			return this.range.contains((ZonedDateTime) left);
		throw new IllegalStateException("Unhandled object type " + left.getClass());
	}

	@Override
	public SearchPredicate coerce(ValueCoercer coercer) {
		// nothing to coerce
		return this;
	}
}
