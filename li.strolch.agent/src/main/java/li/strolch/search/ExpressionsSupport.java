package li.strolch.search;

import li.strolch.model.*;
import li.strolch.model.activity.Activity;
import li.strolch.model.parameter.Parameter;

public class ExpressionsSupport {

	public static <T extends StrolchRootElement> SearchExpression<T> not(SearchExpression<T> expression) {
		return element -> !expression.matches(element);
	}

	public static <T extends StrolchRootElement> ExpressionBuilder<T> id() {
		return StrolchElement::getId;
	}

	public static <T extends StrolchRootElement> SearchExpression<T> id(SearchPredicate predicate) {
		return element -> predicate.matches(element.getId());
	}

	public static <T extends StrolchRootElement> ExpressionBuilder<T> name() {
		return StrolchElement::getName;
	}

	public static <T extends StrolchRootElement> SearchExpression<T> name(SearchPredicate predicate) {
		return element -> predicate.matches(element.getName());
	}

	public static <T extends StrolchRootElement> ExpressionBuilder<T> date() {
		return element -> ((Order) element).getDate();
	}

	public static <T extends StrolchRootElement> SearchExpression<T> date(SearchPredicate predicate) {
		return element -> predicate.matches(((Order) element).getDate());
	}

	public static <T extends StrolchRootElement> ExpressionBuilder<T> state() {
		return element -> {
			if (element instanceof Order)
				return ((Order) element).getState();
			if (element instanceof Activity)
				return ((Activity) element).getState();
			throw new IllegalArgumentException(element.getObjectType() + " does not have a state!");
		};
	}

	public static <T extends StrolchRootElement> SearchExpression<T> state(SearchPredicate predicate) {
		return element -> predicate.matches(state().extract(element));
	}

	public static <T extends StrolchRootElement> ExpressionBuilder<T> param(String bagId, String paramId) {
		return element -> {
			ParameterBag bag = element.getParameterBag(bagId);
			if (bag == null)
				return null;

			Parameter<?> param = bag.getParameter(paramId);
			return param == null ? null : param.getValue();
		};
	}

	public static <T extends StrolchRootElement> SearchExpression<T> param(String bagId, String paramId,
			SearchPredicate predicate) {
		return element -> predicate.matches(param(bagId, paramId).extract(element));
	}

	public static <T extends StrolchRootElement> SearchExpression<T> paramNull(String bagId, String paramId) {
		return element -> {
			ParameterBag bag = element.getParameterBag(bagId);
			if (bag == null)
				return true;

			Parameter<?> param = bag.getParameter(paramId);
			return param == null;
		};
	}
}
