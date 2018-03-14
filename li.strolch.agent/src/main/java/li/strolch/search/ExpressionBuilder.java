package li.strolch.search;

import li.strolch.model.StrolchRootElement;
import li.strolch.utils.collections.DateRange;

public interface ExpressionBuilder {

	Object extract(StrolchRootElement element);

	default SearchExpression isEqualTo(Object right) {
		return element -> PredicatesSupport.isEqualTo(right).matches(extract(element));
	}

	default SearchExpression isNotEqualTo(Object right) {
		return element -> PredicatesSupport.isNotEqualTo(right).matches(extract(element));
	}

	default SearchExpression isEqualToIgnoreCase(Object right) {
		return element -> PredicatesSupport.isEqualToIgnoreCase(right).matches(extract(element));
	}

	default SearchExpression isNotEqualToIgnoreCase(Object right) {
		return element -> PredicatesSupport.isNotEqualToIgnoreCase(right).matches(extract(element));
	}

	default SearchExpression startsWith(Object right) {
		return element -> PredicatesSupport.startsWith(right).matches(extract(element));
	}

	default SearchExpression startsWithIgnoreCase(Object right) {
		return element -> PredicatesSupport.startsWithIgnoreCase(right).matches(extract(element));
	}

	default SearchExpression endsWith(Object right) {
		return element -> PredicatesSupport.endsWith(right).matches(extract(element));
	}

	default SearchExpression endsWithIgnoreCase(Object right) {
		return element -> PredicatesSupport.endsWithIgnoreCase(right).matches(extract(element));
	}

	default SearchExpression contains(Object right) {
		return element -> PredicatesSupport.contains(right).matches(extract(element));
	}

	default SearchExpression containsIgnoreCase(Object right) {
		return element -> PredicatesSupport.containsIgnoreCase(right).matches(extract(element));
	}

	default SearchExpression inRange(DateRange range) {
		return element -> PredicatesSupport.inRange(range).matches(extract(element));
	}
}
