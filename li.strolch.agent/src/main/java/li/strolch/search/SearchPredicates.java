package li.strolch.search;

import java.util.Date;

import li.strolch.utils.collections.DateRange;

public interface SearchPredicates {

	default SearchPredicate isEqualTo(Object right) {
		return PredicatesSupport.isEqualTo(right, false);
	}

	default SearchPredicate isNotEqualTo(Object right) {
		return PredicatesSupport.isEqualTo(right, false).not();
	}

	default SearchPredicate isEqualToIgnoreCase(Object right) {
		return PredicatesSupport.isEqualTo(right, true);
	}

	default SearchPredicate isNotEqualToIgnoreCase(Object right) {
		return PredicatesSupport.isEqualTo(right, true).not();
	}

	default SearchPredicate startsWith(Object right) {
		return PredicatesSupport.startsWith(right, false);
	}

	default SearchPredicate startsWithIgnoreCase(Object right) {
		return PredicatesSupport.startsWith(right, true);
	}

	default SearchPredicate endsWith(Object right) {
		return PredicatesSupport.endsWith(right, false);
	}

	default SearchPredicate endsWithIgnoreCase(Object right) {
		return PredicatesSupport.endsWith(right, true);
	}

	default SearchPredicate contains(Object right) {
		return PredicatesSupport.contains(right, false);
	}

	default SearchPredicate containsIgnoreCase(Object right) {
		return PredicatesSupport.contains(right, true);
	}

	default SearchPredicate inRange(DateRange range) {
		return left -> range.contains((Date) left);
	}
}
