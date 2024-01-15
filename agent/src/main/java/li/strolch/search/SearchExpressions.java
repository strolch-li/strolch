package li.strolch.search;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;

import java.util.function.Function;
import java.util.function.Supplier;

import static li.strolch.model.StrolchModelConstants.BAG_PARAMETERS;
import static li.strolch.model.StrolchModelConstants.BAG_RELATIONS;

/**
 * Declares specific search expressions, i.e. extracting the relevant data for a where clause
 */
public interface SearchExpressions {

	default <T extends StrolchRootElement> SearchExpression<T> not(SearchExpression<T> expression) {
		return element -> !expression.matches(element);
	}

	default <T extends StrolchRootElement> SearchExpression<T> predicate(Supplier<Boolean> predicate) {
		return ExpressionsSupport.predicate(predicate);
	}

	default <T extends StrolchRootElement> SearchExpression<T> predicate(Function<T, Boolean> predicate) {
		return ExpressionsSupport.predicate(predicate);
	}

	default ExpressionBuilder mapResource(Function<Resource, Object> extractor) {
		return t -> extractor.apply((Resource) t);
	}

	default ExpressionBuilder mapOrder(Function<Order, Object> extractor) {
		return t -> extractor.apply((Order) t);
	}

	default ExpressionBuilder mapActivity(Function<Activity, Object> extractor) {
		return t -> extractor.apply((Activity) t);
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

	default ExpressionBuilder param(String paramId) {
		return ExpressionsSupport.param(BAG_PARAMETERS, paramId);
	}

	default <T extends StrolchRootElement> ExpressionBuilder extract(Function<T, Object> extractor) {
		return ExpressionsSupport.extract(extractor);
	}

	default ExpressionBuilder relation(String paramId) {
		return ExpressionsSupport.param(BAG_RELATIONS, paramId);
	}

	default ExpressionBuilder param(String bagId, String paramId) {
		return ExpressionsSupport.param(bagId, paramId);
	}

	default <T extends StrolchRootElement> SearchExpression<T> param(String paramId, SearchPredicate predicate) {
		return ExpressionsSupport.param(BAG_PARAMETERS, paramId, predicate);
	}

	default <T extends StrolchRootElement> SearchExpression<T> param(String bagId, String paramId,
			SearchPredicate predicate) {
		return ExpressionsSupport.param(bagId, paramId, predicate);
	}

	default <T extends StrolchRootElement> SearchExpression<T> paramNull(String paramId) {
		return ExpressionsSupport.paramNull(BAG_PARAMETERS, paramId);
	}

	default <T extends StrolchRootElement> SearchExpression<T> relationNull(String paramId) {
		return ExpressionsSupport.paramNull(BAG_RELATIONS, paramId);
	}

	default <T extends StrolchRootElement> SearchExpression<T> paramNull(String bagId, String paramId) {
		return ExpressionsSupport.paramNull(bagId, paramId);
	}

	default ExpressionBuilder paramOnBagType(String bagType, String paramId) {
		return ExpressionsSupport.paramOnBagType(bagType, paramId);
	}

	default ExpressionBuilder relationName(StrolchTransaction tx, String paramId) {
		return ExpressionsSupport.relationName(tx, paramId);
	}

	default <T extends StrolchRootElement> SearchExpression<T> relationName(StrolchTransaction tx,
			String relationParamId, SearchPredicate predicate) {
		return ExpressionsSupport.relationName(tx, relationParamId, predicate);
	}

	default ExpressionBuilder relationParam(StrolchTransaction tx, String relationParamId, String paramId) {
		return ExpressionsSupport.relationParam(tx, relationParamId, BAG_PARAMETERS, paramId);
	}

	default ExpressionBuilder relationParam(StrolchTransaction tx, String relationParamId, String bagId,
			String paramId) {
		return ExpressionsSupport.relationParam(tx, relationParamId, bagId, paramId);
	}

	default <T extends StrolchRootElement> SearchExpression<T> relationParam(StrolchTransaction tx,
			String relationParamId, String paramId, SearchPredicate predicate) {
		return ExpressionsSupport.relationParam(tx, relationParamId, BAG_PARAMETERS, paramId, predicate);
	}

	default <T extends StrolchRootElement> SearchExpression<T> relationParam(StrolchTransaction tx,
			String relationParamId, String bagId, String paramId, SearchPredicate predicate) {
		return ExpressionsSupport.relationParam(tx, relationParamId, bagId, paramId, predicate);
	}
}
