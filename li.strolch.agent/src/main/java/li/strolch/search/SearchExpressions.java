package li.strolch.search;

import li.strolch.model.StrolchRootElement;

/**
 * Declares specific search expressions, i.e. extracting the relevant data for a where clause
 *
 * @param <T>
 */
public interface SearchExpressions {

	default <T extends StrolchRootElement> SearchExpression<T> not(SearchExpression<T> expression) {
		return element -> !expression.matches(element);
	}

	default ExpressionBuilder id() {
		return ExpressionsSupport.id();
	}

	default <T extends StrolchRootElement> SearchExpression<T> id(SearchPredicate predicate) {
		return ExpressionsSupport.id(predicate);
	}

	default ExpressionBuilder name() {
		return ExpressionsSupport.name();
	}

	default <T extends StrolchRootElement> SearchExpression<T> name(SearchPredicate predicate) {
		return ExpressionsSupport.name(predicate);
	}

	default ExpressionBuilder date() {
		return ExpressionsSupport.date();
	}

	default <T extends StrolchRootElement> SearchExpression<T> date(SearchPredicate predicate) {
		return ExpressionsSupport.date(predicate);
	}

	default ExpressionBuilder state() {
		return ExpressionsSupport.state();
	}

	default <T extends StrolchRootElement> SearchExpression<T> state(SearchPredicate predicate) {
		return ExpressionsSupport.state(predicate);
	}

	default ExpressionBuilder param(String bagId, String paramId) {
		return ExpressionsSupport.param(bagId, paramId);
	}

	default <T extends StrolchRootElement> SearchExpression<T> param(String bagId, String paramId,
			SearchPredicate predicate) {
		return ExpressionsSupport.param(bagId, paramId, predicate);
	}

	default <T extends StrolchRootElement> SearchExpression<T> paramNull(String bagId, String paramId) {
		return ExpressionsSupport.paramNull(bagId, paramId);
	}
}
