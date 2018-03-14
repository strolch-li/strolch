package li.strolch.search;

import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.StrolchElement;
import li.strolch.model.activity.Activity;
import li.strolch.model.parameter.Parameter;

public class ExpressionsSupport {

	public static SearchExpression not(SearchExpression expression) {
		return element -> !expression.matches(element);
	}

	public static ExpressionBuilder id() {
		return StrolchElement::getId;
	}

	public static SearchExpression id(SearchPredicate predicate) {
		return element -> predicate.matches(element.getId());
	}

	public static ExpressionBuilder name() {
		return StrolchElement::getName;
	}

	public static SearchExpression name(SearchPredicate predicate) {
		return element -> predicate.matches(element.getName());
	}

	public static ExpressionBuilder date() {
		return element -> ((Order) element).getDate();
	}

	public static SearchExpression date(SearchPredicate predicate) {
		return element -> predicate.matches(((Order) element).getDate());
	}

	public static ExpressionBuilder state() {
		return element -> {
			if (element instanceof Order)
				return ((Order) element).getState();
			if (element instanceof Activity)
				return ((Activity) element).getState();
			throw new IllegalArgumentException(element.getObjectType() + " does not have a state!");
		};
	}

	public static SearchExpression state(SearchPredicate predicate) {
		return element -> predicate.matches(state().extract(element));
	}

	public static ExpressionBuilder param(String bagId, String paramId) {
		return element -> {
			ParameterBag bag = element.getParameterBag(bagId);
			if (bag == null)
				return null;

			Parameter<?> param = bag.getParameter(paramId);
			return param == null ? null : param.getValue();
		};
	}

	public static SearchExpression param(String bagId, String paramId, SearchPredicate predicate) {
		return element -> predicate.matches(param(bagId, paramId).extract(element));
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
