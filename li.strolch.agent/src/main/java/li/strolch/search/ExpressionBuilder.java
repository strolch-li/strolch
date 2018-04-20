package li.strolch.search;

import li.strolch.model.StrolchRootElement;
import li.strolch.utils.collections.DateRange;

public interface ExpressionBuilder<T extends StrolchRootElement> {

	Object extract(StrolchRootElement element);

	default ValueCoercer getValueCoercer(StrolchRootElement context) {
		return e -> e;
	}

	default SearchExpression<T> isEqualTo(Object right) {
		return element -> PredicatesSupport.isEqualTo(right).matches(extract(element));
	}

	default SearchExpression<T> isNotEqualTo(Object right) {
		return element -> PredicatesSupport.isNotEqualTo(right).matches(extract(element));
	}

	default SearchExpression<T> isEqualToIgnoreCase(Object right) {
		return element -> PredicatesSupport.isEqualToIgnoreCase(right).matches(extract(element));
	}

	default SearchExpression<T> isNotEqualToIgnoreCase(Object right) {
		return element -> PredicatesSupport.isNotEqualToIgnoreCase(right).matches(extract(element));
	}

	default SearchExpression<T> startsWith(Object right) {
		return element -> PredicatesSupport.startsWith(right).matches(extract(element));
	}

	default SearchExpression<T> startsWithIgnoreCase(Object right) {
		return element -> PredicatesSupport.startsWithIgnoreCase(right).matches(extract(element));
	}

	default SearchExpression<T> endsWith(Object right) {
		return element -> PredicatesSupport.endsWith(right).matches(extract(element));
	}

	default SearchExpression<T> endsWithIgnoreCase(Object right) {
		return element -> PredicatesSupport.endsWithIgnoreCase(right).matches(extract(element));
	}

	default SearchExpression<T> contains(Object right) {
		return element -> PredicatesSupport.contains(right).matches(extract(element));
	}

	default SearchExpression<T> containsIgnoreCase(Object right) {
		return element -> PredicatesSupport.containsIgnoreCase(right).matches(extract(element));
	}

	default SearchExpression<T> isIn(Object right) {
		return element -> PredicatesSupport.isIn(right).matches(extract(element));
	}

	default SearchExpression<T> isInIgnoreCase(Object right) {
		return element -> PredicatesSupport.isInIgnoreCase(right).matches(extract(element));
	}

	default SearchExpression<T> inRange(DateRange range) {
		return element -> PredicatesSupport.inRange(range).matches(extract(element));
	}
}
