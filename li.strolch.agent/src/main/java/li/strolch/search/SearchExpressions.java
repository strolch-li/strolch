package li.strolch.search;

import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.activity.Activity;
import li.strolch.model.parameter.Parameter;

public class SearchExpressions {

	public static SearchExpression id(SearchPredicate predicate) {
		return element -> predicate.matches(element.getId());
	}

	public static SearchExpression name(SearchPredicate predicate) {
		return element -> predicate.matches(element.getName());
	}

	public static SearchExpression date(SearchPredicate predicate) {
		return element -> predicate.matches(((Order) element).getDate());
	}

	public static SearchExpression state(SearchPredicate predicate) {
		return element -> {
			if (element instanceof Order)
				return predicate.matches(((Order) element).getState());
			if (element instanceof Activity)
				return predicate.matches(((Activity) element).getState());
			throw new IllegalArgumentException(element.getObjectType() + " does not have a state!");
		};
	}

	public static SearchExpression param(String bagId, String paramId, SearchPredicate predicate) {
		return element -> {
			ParameterBag bag = element.getParameterBag(bagId);
			if (bag == null)
				return false;

			Parameter<?> param = bag.getParameter(paramId);
			return param != null && predicate.matches(param.getValue());
		};
	}

	public static SearchExpression paramNull(String bagId, String paramId) {
		return element -> {
			ParameterBag bag = element.getParameterBag(bagId);
			if (bag == null)
				return true;

			Parameter<?> param = bag.getParameter(paramId);
			return param == null;
		};
	}
}
