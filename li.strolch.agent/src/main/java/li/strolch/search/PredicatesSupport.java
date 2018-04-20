package li.strolch.search;

import li.strolch.search.predicates.*;
import li.strolch.utils.collections.DateRange;

public class PredicatesSupport {

	public static SearchPredicate isEqualTo(Object right) {
		return new IsEqualToPredicate(right, false);
	}

	public static SearchPredicate isEqualToIgnoreCase(Object right) {
		return new IsEqualToPredicate(right, true);
	}

	public static SearchPredicate isNotEqualTo(Object right) {
		return new IsEqualToPredicate(right, false).not();
	}

	public static SearchPredicate isNotEqualToIgnoreCase(Object right) {
		return new IsEqualToPredicate(right, true).not();
	}

	public static SearchPredicate startsWith(Object right) {
		return new StartsWithPredicate(right, false);
	}

	public static SearchPredicate startsWithIgnoreCase(Object right) {
		return new StartsWithPredicate(right, true);
	}

	public static SearchPredicate endsWith(Object right) {
		return new EndsWithPredicate(right, false);
	}

	public static SearchPredicate endsWithIgnoreCase(Object right) {
		return new EndsWithPredicate(right, true);
	}

	public static SearchPredicate contains(Object right) {
		return new ContainsPredicate(right, false);
	}

	public static SearchPredicate containsIgnoreCase(Object right) {
		return new ContainsPredicate(right, true);
	}

	public static SearchPredicate isIn(Object right) {
		return new IsInPredicate(right, false);
	}

	public static SearchPredicate isInIgnoreCase(Object right) {
		return new IsInPredicate(right, true);
	}

	public static SearchPredicate inRange(DateRange range) {
		return new InRangePredicate(range);
	}
}
