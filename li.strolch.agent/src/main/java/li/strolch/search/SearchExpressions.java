package li.strolch.search;

import li.strolch.model.StrolchRootElement;

/**
 * Declares specific search expressions, i.e. extracting the relevant data for a where clause
 *
 * @param <T>
 */
public interface SearchExpressions<T extends StrolchRootElement> {

	default SearchExpression<T> not(SearchExpression<T> expression) {
		return element -> !expression.matches(element);
	}

	default ExpressionBuilder<T> id() {
		return ExpressionsSupport.id();
	}

	default SearchExpression<T> id(SearchPredicate predicate) {
		return ExpressionsSupport.id(predicate);
	}

	default ExpressionBuilder<T> name() {
		return ExpressionsSupport.name();
	}

	default SearchExpression<T> name(SearchPredicate predicate) {
		return ExpressionsSupport.name(predicate);
	}

	default ExpressionBuilder<T> date() {
		return ExpressionsSupport.date();
	}

	default SearchExpression<T> date(SearchPredicate predicate) {
		return ExpressionsSupport.date(predicate);
	}

	default ExpressionBuilder<T> state() {
		return ExpressionsSupport.state();
	}

	default SearchExpression<T> state(SearchPredicate predicate) {
		return ExpressionsSupport.state(predicate);
	}

	default ExpressionBuilder<T> param(String bagId, String paramId) {
		return ExpressionsSupport.param(bagId, paramId);
	}

	default SearchExpression<T> param(String bagId, String paramId, SearchPredicate predicate) {
		return ExpressionsSupport.param(bagId, paramId, predicate);
	}

	default SearchExpression<T> paramNull(String bagId, String paramId) {
		return ExpressionsSupport.paramNull(bagId, paramId);
	}
}
