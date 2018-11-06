package li.strolch.search;

import static li.strolch.search.ExpressionsSupport.*;
import static li.strolch.search.PredicatesSupport.containsIgnoreCase;
import static li.strolch.search.PredicatesSupport.isEqualTo;
import static li.strolch.utils.helper.StringHelper.trimOrEmpty;

import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.utils.helper.StringHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>A helper class to build search expressions on {@link StrolchRootElement}</p>
 *
 * <p>This is often used in a web context where searches are performed on multiple parameters etc. of an element</p>
 *
 * <p>Note that the query string is parsed using the following rules:</p>
 * <ul>
 * <li>query is trimmed</li>
 * <li>empty query means search for everything, i.e. no {@link SearchExpression SearchExpressions} are added</li>
 * <li>query is split by space, and each part is handled further:</li>
 * <li>format {@code param:<bagId>:<paramId>:<value>} adds search expression for given
 * bag/param</li>
 * <li>otherwise search expression for id and name are added</li>
 * <li>all added search expressions are ANDed</li>
 * </ul>
 */
public class SearchBuilder {

	private static final Logger logger = LoggerFactory.getLogger(SearchBuilder.class);

	/**
	 * Builds an {@link OrderSearch} for the given types with the given query
	 *
	 * @param query
	 * 		the query
	 * @param types
	 * 		the type of orders to search
	 *
	 * @return the {@link OrderSearch}
	 */
	public static OrderSearch buildOrderSearch(String query, String... types) {
		return buildSearch(new OrderSearch().types(types), query);
	}

	/**
	 * Builds an {@link ResourceSearch} for the given types with the given query
	 *
	 * @param query
	 * 		the query
	 * @param types
	 * 		the type of resources to search
	 *
	 * @return the {@link OrderSearch}
	 */
	public static ResourceSearch buildResourceSearch(String query, String... types) {
		return buildSearch(new ResourceSearch().types(types), query);
	}

	/**
	 * Builds an {@link ActivitySearch} for the given types with the given query
	 *
	 * @param query
	 * 		the query
	 * @param types
	 * 		the type of activities to search
	 *
	 * @return the {@link OrderSearch}
	 */
	public static ActivitySearch buildActivitySearch(String query, String... types) {
		return buildSearch(new ActivitySearch().types(types), query);
	}

	@SuppressWarnings("unchecked")
	private static <T extends StrolchRootElement, U extends StrolchSearch<T>> U buildSearch(U search, String query) {
		query = trimOrEmpty(query);

		if (query.isEmpty())
			return search;

		boolean and = true;
		if (query.startsWith("&")) {
			query = query.substring(1);
		} else if (query.startsWith("and ")) {
			query = query.substring("and ".length());
		} else if (query.startsWith("|")) {
			and = false;
			query = query.substring(1);
		} else if (query.startsWith("or ")) {
			and = false;
			query = query.substring("or ".length());
		}

		query = trimOrEmpty(query);

		SearchExpression<T> se = null;

		String[] parts = query.split(" ");
		for (String part : parts) {
			part = part.trim();

			boolean negate = false;
			if (part.startsWith("!")) {
				negate = true;
				part = part.substring(1);
			}

			String[] paramParts = part.split(":", -1);
			if (paramParts.length != 3) {
				se = add(and, negate, se, id(containsIgnoreCase(part)).or(name(containsIgnoreCase(part))));
			} else {
				String bagId = paramParts[0];
				String paramId = paramParts[1];
				String value = paramParts[2];

				SearchPredicate predicate;
				if (value.isEmpty())
					predicate = isEqualTo(value);
				else
					predicate = containsIgnoreCase(value);

				se = add(and, negate, se, param(bagId, paramId, predicate));
			}
		}

		if (se == null)
			throw new IllegalArgumentException("search expression not evaluated for string " + query);

		search = (U) search.where(se);
		return search;
	}

	private static <T extends StrolchRootElement> SearchExpression<T> add(boolean and, boolean negate,
			SearchExpression<T> se, SearchExpression<T> expression) {

		if (negate)
			expression = expression.not();

		if (se == null)
			return expression;

		if (and)
			return se.and(expression);
		else
			return se.or(expression);
	}

	public static <T extends StrolchRootElement> RootElementSearchResult<T> orderBy(
			RootElementSearchResult<T> searchResult, String orderBy, boolean descending) {

		if (StringHelper.isEmpty(orderBy))
			return searchResult.orderById(descending);

		switch (orderBy) {
		case Tags.Json.ID:
			return searchResult.orderById(descending);
		case Tags.Json.NAME:
			return searchResult.orderByName(descending);
		default:
			logger.warn("Unhandled ordering " + orderBy);
			break;
		}

		return searchResult;
	}
}
