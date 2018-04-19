package li.strolch.search;

import li.strolch.utils.collections.DateRange;

public interface SearchPredicates {

	default SearchPredicate isEqualTo(Object right) {
		return PredicatesSupport.isEqualTo(right);
	}

	default SearchPredicate isNotEqualTo(Object right) {
		return PredicatesSupport.isNotEqualTo(right);
	}

	default SearchPredicate isEqualToIgnoreCase(Object right) {
		return PredicatesSupport.isEqualToIgnoreCase(right);
	}

	default SearchPredicate isNotEqualToIgnoreCase(Object right) {
		return PredicatesSupport.isNotEqualToIgnoreCase(right);
	}

	default SearchPredicate startsWith(Object right) {
		return PredicatesSupport.startsWith(right);
	}

	default SearchPredicate startsWithIgnoreCase(Object right) {
		return PredicatesSupport.startsWithIgnoreCase(right);
	}

	default SearchPredicate endsWith(Object right) {
		return PredicatesSupport.endsWith(right);
	}

	default SearchPredicate endsWithIgnoreCase(Object right) {
		return PredicatesSupport.endsWithIgnoreCase(right);
	}

	default SearchPredicate contains(Object right) {
		return PredicatesSupport.contains(right);
	}

	default SearchPredicate containsIgnoreCase(Object right) {
		return PredicatesSupport.containsIgnoreCase(right);
	}

	default SearchPredicate isIn(Object right) {
		return PredicatesSupport.isIn(right);
	}

	default SearchPredicate isIn(Object... right) {
		return PredicatesSupport.isIn(right);
	}

	default SearchPredicate isInIgnoreCase(Object right) {
		return PredicatesSupport.isInIgnoreCase(right);
	}

	default SearchPredicate isInIgnoreCase(Object... right) {
		return PredicatesSupport.isInIgnoreCase(right);
	}

	default SearchPredicate inRange(DateRange range) {
		return PredicatesSupport.inRange(range);
	}
}
