package li.strolch.search.predicates;

import li.strolch.search.SearchPredicate;
import li.strolch.search.ValueCoercer;

import java.time.ZonedDateTime;
import java.util.Date;

/**
 * <p>A date predicate, concrete classes implement matching.</p>
 *
 * <b>Note:</b> Can only be used with {@link Date} elements
 */
public abstract class DatePredicate implements SearchPredicate {

	protected final ZonedDateTime dateTime;
	protected final boolean inclusive;

	public DatePredicate(ZonedDateTime dateTime, boolean inclusive) {
		this.dateTime = dateTime;
		this.inclusive = inclusive;
	}

	@Override
	public SearchPredicate coerce(ValueCoercer coercer) {
		// nothing to coerce
		return this;
	}
}
