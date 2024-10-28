package li.strolch.search.predicates;

import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;

/**
 * <p>Implements the date is after predicate.</p>
 *
 * <b>Note:</b> Can only be used with {@link Date} or {@link ZonedDateTime} objects
 */
public class IsAfterPredicate extends DatePredicate {
	public IsAfterPredicate(ZonedDateTime dateTime, boolean inclusive) {
		super(dateTime, inclusive);
	}

	@Override
	public boolean matches(Object left) {
		if (left instanceof Date other) {
			ZonedDateTime zdt = ZonedDateTime.ofInstant(other.toInstant(), ZoneId.systemDefault());
			if (this.inclusive && this.dateTime.isEqual(zdt))
				return true;
			return zdt.isAfter(this.dateTime);
		} else if (left instanceof ZonedDateTime zdt) {
			if (this.inclusive && this.dateTime.isEqual(zdt))
				return true;
			return zdt.isAfter(this.dateTime);
		}
		throw new IllegalStateException("Unhandled object type " + left.getClass());
	}
}
