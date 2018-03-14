package li.strolch.search;

public interface SearchExpressions {

	default SearchExpression not(SearchExpression expression) {
		return element -> !expression.matches(element);
	}

	default ExpressionBuilder id() {
		return ExpressionsSupport.id();
	}

	default SearchExpression id(SearchPredicate predicate) {
		return ExpressionsSupport.id(predicate);
	}

	default ExpressionBuilder name() {
		return ExpressionsSupport.name();
	}

	default SearchExpression name(SearchPredicate predicate) {
		return ExpressionsSupport.name(predicate);
	}

	default ExpressionBuilder date() {
		return ExpressionsSupport.date();
	}

	default SearchExpression date(SearchPredicate predicate) {
		return ExpressionsSupport.date(predicate);
	}

	default ExpressionBuilder state() {
		return ExpressionsSupport.state();
	}

	default SearchExpression state(SearchPredicate predicate) {
		return ExpressionsSupport.state(predicate);
	}

	default ExpressionBuilder param(String bagId, String paramId) {
		return ExpressionsSupport.param(bagId, paramId);
	}

	default SearchExpression param(String bagId, String paramId, SearchPredicate predicate) {
		return ExpressionsSupport.param(bagId, paramId, predicate);
	}

	default SearchExpression paramNull(String bagId, String paramId) {
		return ExpressionsSupport.paramNull(bagId, paramId);
	}
}
