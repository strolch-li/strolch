package li.strolch.search;

import static li.strolch.search.ExpressionsSupport.*;
import static li.strolch.search.PredicatesSupport.containsIgnoreCase;
import static li.strolch.utils.helper.StringHelper.trimOrEmpty;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.model.activity.Activity;
import li.strolch.utils.helper.StringHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SearchBuilder {

	private static final Logger logger = LoggerFactory.getLogger(SearchBuilder.class);

	public static GenericSearch<Order> buildOrderSearch(String query, String... types) {
		return buildSearch(new GenericSearch<Order>().orders(types), query);
	}

	public static GenericSearch<Resource> buildResourceSearch(String query, String... types) {
		return buildSearch(new GenericSearch<Resource>().resources(types), query);
	}

	public static GenericSearch<Activity> buildActivitySearch(String query, String... types) {
		return buildSearch(new GenericSearch<Activity>().activities(types), query);
	}

	private static <T extends StrolchRootElement> GenericSearch<T> buildSearch(GenericSearch<T> search, String query) {
		query = trimOrEmpty(query);

		if (query.isEmpty())
			return search;

		SearchExpression se = null;

		String[] parts = query.split(" ");
		for (String part : parts) {

			if (!part.startsWith("param:")) {

				if (se == null)
					se = id(containsIgnoreCase(part)).or(name(containsIgnoreCase(part)));
				else
					se = se.or(id(containsIgnoreCase(part))).or(name(containsIgnoreCase(part)));

			} else {
				String[] paramParts = part.split(":");
				if (paramParts.length != 4) {

					if (se == null)
						se = id(containsIgnoreCase(part)).or(name(containsIgnoreCase(part)));
					else
						se = se.or(id(containsIgnoreCase(part))).or(name(containsIgnoreCase(part)));

				} else {

					String bagId = paramParts[1];
					String paramId = paramParts[2];
					String value = paramParts[3];

					if (se == null)
						se = param(bagId, paramId, containsIgnoreCase(value));
					else
						se = se.or(param(bagId, paramId, containsIgnoreCase(value)));
				}
			}
		}

		if (se == null)
			throw new IllegalArgumentException("search expression not evaluated for string " + query);

		return search.where(se);
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
