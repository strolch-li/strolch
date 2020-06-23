package li.strolch.search;

import li.strolch.model.StrolchRootElement;
import li.strolch.utils.collections.DateRange;

/**
 * An interface to add search expressions to easily discover the possible search expressions
 */
public interface ExpressionBuilder {

	Object extract(StrolchRootElement element);

	default ValueCoercer getValueCoercer(StrolchRootElement context) {
		return e -> e;
	}

	default <T extends StrolchRootElement> SearchExpression<T> isEmpty() {
		return element -> PredicatesSupport.isEmpty().matches(extract(element));
	}

	default <T extends StrolchRootElement> SearchExpression<T> isEqualTo(Object right) {
		return element -> PredicatesSupport.isEqualTo(right).matches(extract(element));
	}

	default <T extends StrolchRootElement> SearchExpression<T> isNotEqualTo(Object right) {
		return element -> PredicatesSupport.isNotEqualTo(right).matches(extract(element));
	}

	default <T extends StrolchRootElement> SearchExpression<T> isEqualToIgnoreCase(Object right) {
		return element -> PredicatesSupport.isEqualToIgnoreCase(right).matches(extract(element));
	}

	default <T extends StrolchRootElement> SearchExpression<T> isNotEqualToIgnoreCase(Object right) {
		return element -> PredicatesSupport.isNotEqualToIgnoreCase(right).matches(extract(element));
	}

	default <T extends StrolchRootElement> SearchExpression<T> startsWith(Object right) {
		return element -> PredicatesSupport.startsWith(right).matches(extract(element));
	}

	default <T extends StrolchRootElement> SearchExpression<T> startsWithIgnoreCase(Object right) {
		return element -> PredicatesSupport.startsWithIgnoreCase(right).matches(extract(element));
	}

	default <T extends StrolchRootElement> SearchExpression<T> endsWith(Object right) {
		return element -> PredicatesSupport.endsWith(right).matches(extract(element));
	}

	default <T extends StrolchRootElement> SearchExpression<T> endsWithIgnoreCase(Object right) {
		return element -> PredicatesSupport.endsWithIgnoreCase(right).matches(extract(element));
	}

	default <T extends StrolchRootElement> SearchExpression<T> contains(Object right) {
		return element -> PredicatesSupport.contains(right).matches(extract(element));
	}

	default <T extends StrolchRootElement> SearchExpression<T> collectionContains(Object right) {
		return element -> PredicatesSupport.collectionContains(right).matches(extract(element));
	}

	default <T extends StrolchRootElement> SearchExpression<T> containsIgnoreCase(Object right) {
		return element -> PredicatesSupport.containsIgnoreCase(right).matches(extract(element));
	}

	default <T extends StrolchRootElement> SearchExpression<T> isIn(Object right) {
		return element -> PredicatesSupport.isIn(right).matches(extract(element));
	}

	default <T extends StrolchRootElement> SearchExpression<T> isInIgnoreCase(Object right) {
		return element -> PredicatesSupport.isInIgnoreCase(right).matches(extract(element));
	}

	default <T extends StrolchRootElement> SearchExpression<T> inRange(DateRange range) {
		return element -> PredicatesSupport.inRange(range).matches(extract(element));
	}
}
