package li.strolch.search;

import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.StrolchElement;
import li.strolch.model.activity.Activity;
import li.strolch.model.parameter.Parameter;

public interface SearchExpressions {

	default SearchExpression not(SearchExpression expression) {
		return element -> !expression.matches(element);
	}

	default ExpressionBuilder id() {
		return StrolchElement::getId;
	}

	default SearchExpression id(SearchPredicate predicate) {
		return element -> predicate.matches(element.getId());
	}

	default ExpressionBuilder name() {
		return StrolchElement::getName;
	}

	default SearchExpression name(SearchPredicate predicate) {
		return element -> predicate.matches(element.getName());
	}

	default ExpressionBuilder date() {
		return element -> ((Order) element).getDate();
	}

	default SearchExpression date(SearchPredicate predicate) {
		return element -> predicate.matches(((Order) element).getDate());
	}

	default ExpressionBuilder state() {
		return element -> {
			if (element instanceof Order)
				return ((Order) element).getState();
			if (element instanceof Activity)
				return ((Activity) element).getState();
			throw new IllegalArgumentException(element.getObjectType() + " does not have a state!");
		};
	}

	default SearchExpression state(SearchPredicate predicate) {
		return element -> predicate.matches(state().extract(element));
	}

	default ExpressionBuilder param(String bagId, String paramId) {
		return element -> {
			ParameterBag bag = element.getParameterBag(bagId);
			if (bag == null)
				return null;

			Parameter<?> param = bag.getParameter(paramId);
			return param == null ? null : param.getValue();
		};
	}

	default SearchExpression param(String bagId, String paramId, SearchPredicate predicate) {
		return element -> predicate.matches(param(bagId, paramId).extract(element));
	}

	default SearchExpression paramNull(String bagId, String paramId) {
		return element -> {
			ParameterBag bag = element.getParameterBag(bagId);
			if (bag == null)
				return true;

			Parameter<?> param = bag.getParameter(paramId);
			return param == null;
		};
	}
}
