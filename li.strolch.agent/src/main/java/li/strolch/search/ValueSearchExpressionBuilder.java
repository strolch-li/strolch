package li.strolch.search;

import java.util.Date;
import java.util.function.Function;

import li.strolch.utils.collections.DateRange;

public class ValueSearchExpressionBuilder {

	public static <T> ValueSearchExpression<T> isEqualTo(Function<T, Object> extractor, Object right) {
		return element -> PredicatesSupport.isEqualTo(right).matches(extractor.apply(element));
	}

	public static <T> ValueSearchExpression<T> isNotEqualTo(Function<T, Object> extractor, Object right) {
		return element -> PredicatesSupport.isNotEqualTo(right).matches(extractor.apply(element));
	}

	public static <T> ValueSearchExpression<T> isEqualToIgnoreCase(Function<T, Object> extractor, Object right) {
		return element -> PredicatesSupport.isEqualToIgnoreCase(right).matches(extractor.apply(element));
	}

	public static <T> ValueSearchExpression<T> isNotEqualToIgnoreCase(Function<T, Object> extractor, Object right) {
		return element -> PredicatesSupport.isNotEqualToIgnoreCase(right).matches(extractor.apply(element));
	}

	public static <T> ValueSearchExpression<T> startsWith(Function<T, Object> extractor, Object right) {
		return element -> PredicatesSupport.startsWith(right).matches(extractor.apply(element));
	}

	public static <T> ValueSearchExpression<T> startsWithIgnoreCase(Function<T, Object> extractor, Object right) {
		return element -> PredicatesSupport.startsWithIgnoreCase(right).matches(extractor.apply(element));
	}

	public static <T> ValueSearchExpression<T> endsWith(Function<T, Object> extractor, Object right) {
		return element -> PredicatesSupport.endsWith(right).matches(extractor.apply(element));
	}

	public static <T> ValueSearchExpression<T> endsWithIgnoreCase(Function<T, Object> extractor, Object right) {
		return element -> PredicatesSupport.endsWithIgnoreCase(right).matches(extractor.apply(element));
	}

	public static <T> ValueSearchExpression<T> contains(Function<T, Object> extractor, Object right) {
		return element -> PredicatesSupport.contains(right).matches(extractor.apply(element));
	}

	public static <T> ValueSearchExpression<T> collectionContains(Function<T, Object> extractor, Object right) {
		return element -> PredicatesSupport.collectionContains(right).matches(extractor.apply(element));
	}

	public static <T> ValueSearchExpression<T> containsIgnoreCase(Function<T, Object> extractor, Object right) {
		return element -> PredicatesSupport.containsIgnoreCase(right).matches(extractor.apply(element));
	}

	public static <T> ValueSearchExpression<T> isIn(Function<T, Object> extractor, Object right) {
		return element -> PredicatesSupport.isIn(right).matches(extractor.apply(element));
	}

	public static <T> ValueSearchExpression<T> isInIgnoreCase(Function<T, Object> extractor, Object right) {
		return element -> PredicatesSupport.isInIgnoreCase(right).matches(extractor.apply(element));
	}

	public static <T> ValueSearchExpression<T> inRange(Function<T, Date> extractor, DateRange range) {
		return element -> PredicatesSupport.inRange(range).matches(extractor.apply(element));
	}
}
