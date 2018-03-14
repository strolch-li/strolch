package li.strolch.search;

import java.util.Date;

import li.strolch.model.StrolchRootElement;
import li.strolch.utils.collections.DateRange;

public interface ExpressionBuilder {

	Object extract(StrolchRootElement element);

	default SearchExpression isEqualTo(Object right) {
		return element -> PredicatesSupport.isEqualTo(right, false).matches(extract(element));
	}

	default SearchExpression isNotEqualTo(Object right) {
		return element -> PredicatesSupport.isEqualTo(right, false).not().matches(extract(element));
	}

	default SearchExpression isEqualToIgnoreCase(Object right) {
		return element -> PredicatesSupport.isEqualTo(right, true).matches(extract(element));
	}

	default SearchExpression isNotEqualToIgnoreCase(Object right) {
		return element -> PredicatesSupport.isEqualTo(right, true).not().matches(extract(element));
	}

	default SearchExpression startsWith(Object right) {
		return element -> PredicatesSupport.startsWith(right, false).matches(extract(element));
	}

	default SearchExpression startsWithIgnoreCase(Object right) {
		return element -> PredicatesSupport.startsWith(right, true).matches(extract(element));
	}

	default SearchExpression endsWith(Object right) {
		return element -> PredicatesSupport.endsWith(right, false).matches(extract(element));
	}

	default SearchExpression endsWithIgnoreCase(Object right) {
		return element -> PredicatesSupport.endsWith(right, true).matches(extract(element));
	}

	default SearchExpression contains(Object right) {
		return element -> PredicatesSupport.contains(right, false).matches(extract(element));
	}

	default SearchExpression containsIgnoreCase(Object right) {
		return element -> PredicatesSupport.contains(right, true).matches(extract(element));
	}

	default SearchExpression inRange(DateRange range) {
		return element -> range.contains((Date) extract(element));
	}
}
