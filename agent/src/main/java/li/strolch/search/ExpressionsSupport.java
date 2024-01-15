package li.strolch.search;

import li.strolch.model.*;
import li.strolch.model.activity.Activity;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

import static li.strolch.model.StrolchModelConstants.*;

/**
 * Implements search expressions to be statically imported when writing searches
 */
public class ExpressionsSupport {

	public static <T extends StrolchRootElement> SearchExpression<T> not(SearchExpression<T> expression) {
		return element -> !expression.matches(element);
	}

	public static <T extends StrolchRootElement> SearchExpression<T> predicate(Supplier<Boolean> predicate) {
		return element -> predicate.get();
	}

	public static <T extends StrolchRootElement> SearchExpression<T> predicate(Function<T, Boolean> predicate) {
		return predicate::apply;
	}

	public static ExpressionBuilder mapResource(Function<Resource, Object> extractor) {
		return t -> extractor.apply((Resource) t);
	}

	public static ExpressionBuilder mapOrder(Function<Order, Object> extractor) {
		return t -> extractor.apply((Order) t);
	}

	public static ExpressionBuilder mapActivity(Function<Activity, Object> extractor) {
		return t -> extractor.apply((Activity) t);
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

	public static <T extends StrolchRootElement> ExpressionBuilder param(String paramId) {
		return param(BAG_PARAMETERS, paramId);
	}

	public static <T extends StrolchRootElement> ExpressionBuilder relationParam(String paramId) {
		return param(BAG_RELATIONS, paramId);
	}

	public static <T extends StrolchRootElement> ExpressionBuilder param(String bagId, String paramId) {
		return new ExpressionBuilder() {

			@Override
			public ValueCoercer getValueCoercer(StrolchRootElement context) {
				return e -> {
					if (!(e instanceof String))
						return e;

					return getParamValue(e, context, bagId, paramId);
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

	public static <T extends StrolchRootElement> ExpressionBuilder paramOnBagType(String bagType, String paramId) {
		return element -> {
			List<Object> result = element
					.streamOfParameterBagsByType(bagType)
					.filter(b -> b.isParamSet(paramId))
					.map(b -> b.getParameter(paramId, true).getValue())
					.toList();
			if (result.size() == 1)
				return result.getFirst();
			return result.toArray();
		};
	}

	public static <T extends StrolchRootElement> ExpressionBuilder extract(Function<T, Object> extractor) {
		return element -> {
			@SuppressWarnings("unchecked") T e = (T) element;
			return extractor.apply(e);
		};
	}

	public static <T extends StrolchRootElement> SearchExpression<T> paramNull(String paramId) {
		return paramNull(BAG_PARAMETERS, paramId);
	}

	public static <T extends StrolchRootElement> SearchExpression<T> relationNull(String paramId) {
		return paramNull(BAG_RELATIONS, paramId);
	}

	public static <T extends StrolchRootElement> SearchExpression<T> paramNull(String bagId, String paramId) {
		return element -> !element.hasParameter(bagId, paramId);
	}

	public static <T extends StrolchRootElement> SearchExpression<T> relationName(StrolchTransaction tx,
			String relationParamId, SearchPredicate predicate) {
		ExpressionBuilder eb = relationName(tx, relationParamId);
		return element -> predicate.coerce(eb.getValueCoercer(element)).matches(eb.extract(element));
	}

	public static <T extends StrolchRootElement> ExpressionBuilder relationName(StrolchTransaction tx,
			String relationParamId) {
		return new ExpressionBuilder() {

			@Override
			public ValueCoercer getValueCoercer(StrolchRootElement context) {
				return e -> {
					if (!(e instanceof String))
						return e;

					StrolchRootElement relation = getRelation(context, tx, relationParamId);
					return relation == null ? e : relation.getName();
				};
			}

			@Override
			public Object extract(StrolchRootElement element) {
				StrolchRootElement relation = getRelation(element, tx, relationParamId);
				return relation == null ? null : relation.getName();
			}
		};
	}

	public static <T extends StrolchRootElement> SearchExpression<T> relationParam(StrolchTransaction tx,
			String relationParamId, String bagId, String paramId, SearchPredicate predicate) {
		ExpressionBuilder eb = relationParam(tx, relationParamId, bagId, paramId);
		return element -> predicate.coerce(eb.getValueCoercer(element)).matches(eb.extract(element));
	}

	public static <T extends StrolchRootElement> ExpressionBuilder relationParam(StrolchTransaction tx,
			String relationParamId, String bagId, String paramId) {
		return new ExpressionBuilder() {

			@Override
			public ValueCoercer getValueCoercer(StrolchRootElement context) {
				return e -> {
					if (!(e instanceof String))
						return e;

					StrolchRootElement relation = getRelation(context, tx, relationParamId);
					if (relation == null)
						return e;

					return getParamValue(e, relation, bagId, paramId);
				};
			}

			@Override
			public Object extract(StrolchRootElement element) {
				StrolchRootElement relation = getRelation(element, tx, relationParamId);
				if (relation == null)
					return null;

				ParameterBag bag = relation.getParameterBag(bagId);
				if (bag == null)
					return null;

				Parameter<?> param = bag.getParameter(paramId);
				return param == null ? null : param.getValue();
			}
		};
	}

	private static Object getParamValue(Object e, StrolchRootElement relation, String bagId, String paramId) {
		ParameterBag bag = relation.getParameterBag(bagId);
		if (bag == null)
			return e;

		Parameter<?> param = bag.getParameter(paramId);
		if (param == null)
			return e;

		return param.getValueType().parseValue((String) e);
	}

	private static StrolchRootElement getRelation(StrolchRootElement element, StrolchTransaction tx, String paramId) {

		ParameterBag bag = element.getParameterBag(BAG_RELATIONS);
		if (bag == null)
			return null;

		Parameter<?> param = bag.getParameter(paramId);
		if (param == null || param.isEmpty() || !StrolchValueType.STRING.getType().equals(param.getType()))
			return null;

		StrolchRootElement relation;
		return switch (param.getInterpretation()) {
			case INTERPRETATION_RESOURCE_REF -> tx.getResourceBy((StringParameter) param);
			case INTERPRETATION_ORDER_REF -> tx.getOrderBy((StringParameter) param);
			case INTERPRETATION_ACTIVITY_REF -> tx.getActivityBy((StringParameter) param);
			default -> null;
		};

	}
}
