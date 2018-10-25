package li.strolch.search;

import li.strolch.model.*;
import li.strolch.model.activity.Activity;
import li.strolch.model.parameter.Parameter;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

/**
 * Implements search expressions to be statically imported when writing searches
 */
public class ExpressionsSupport {

	public static <T extends StrolchRootElement> SearchExpression<T> not(SearchExpression<T> expression) {
		return element -> !expression.matches(element);
	}

	public static <T extends StrolchRootElement> SearchExpression<T> id(SearchPredicate predicate) {
		return element -> predicate.matches(element.getId());
	}

	public static ExpressionBuilder id() {
		return StrolchElement::getId;
	}

	public static <T extends StrolchRootElement> SearchExpression<T> name(SearchPredicate predicate) {
		return element -> predicate.matches(element.getName());
	}

	public static <T extends StrolchRootElement> ExpressionBuilder name() {
		return StrolchElement::getName;
	}

	public static <T extends StrolchRootElement> SearchExpression<T> date(SearchPredicate predicate) {
		ExpressionBuilder eb = date();
		return element -> predicate.coerce(eb.getValueCoercer(element)).matches(eb);
	}

	public static <T extends StrolchRootElement> ExpressionBuilder date() {
		return new ExpressionBuilder() {

			@Override
			public ValueCoercer getValueCoercer(StrolchRootElement context) {
				return e -> {
					if (!(e instanceof String))
						return e;
					return ISO8601FormatFactory.getInstance().parseDate((String) e);
				};
			}

			@Override
			public Object extract(StrolchRootElement element) {
				return ((Order) element).getDate();
			}
		};
	}

	public static <T extends StrolchRootElement> SearchExpression<T> state(SearchPredicate predicate) {
		ExpressionBuilder eb = state();
		return element -> predicate.coerce(eb.getValueCoercer(element)).matches(eb.extract(element));
	}

	public static ExpressionBuilder state() {
		return new ExpressionBuilder() {

			@Override
			public ValueCoercer getValueCoercer(StrolchRootElement context) {
				return e -> {
					if (!(e instanceof String))
						return e;
					return State.parse((String) e);
				};
			}

			@Override
			public Object extract(StrolchRootElement element) {
				if (element instanceof Order)
					return ((Order) element).getState();
				if (element instanceof Activity)
					return ((Activity) element).getState();
				throw new IllegalArgumentException(element.getObjectType() + " does not have a state!");
			}
		};
	}

	public static <T extends StrolchRootElement> SearchExpression<T> param(String bagId, String paramId,
			SearchPredicate predicate) {
		ExpressionBuilder eb = param(bagId, paramId);
		return element -> predicate.coerce(eb.getValueCoercer(element)).matches(eb.extract(element));
	}

	public static <T extends StrolchRootElement> ExpressionBuilder param(String bagId, String paramId) {
		return new ExpressionBuilder() {

			@Override
			public ValueCoercer getValueCoercer(StrolchRootElement context) {
				return e -> {
					if (!(e instanceof String))
						return e;

					ParameterBag bag = context.getParameterBag(bagId);
					if (bag == null)
						return e;

					Parameter<?> param = bag.getParameter(paramId);
					if (param == null)
						return e;

					return param.getValueType().parseValue((String) e);
				};
			}

			@Override
			public Object extract(StrolchRootElement element) {
				ParameterBag bag = element.getParameterBag(bagId);
				if (bag == null)
					return null;

				Parameter<?> param = bag.getParameter(paramId);
				return param == null ? null : param.getValue();
			}
		};
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
