package li.strolch.search;

import static li.strolch.search.ExpressionsSupport.*;
import static li.strolch.search.PredicatesSupport.containsIgnoreCase;
import static li.strolch.utils.helper.StringHelper.trimOrEmpty;

import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.utils.helper.StringHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SearchBuilder {

	private static final Logger logger = LoggerFactory.getLogger(SearchBuilder.class);

	public static OrderSearch buildOrderSearch(String query, String... types) {
		return buildSearch(new OrderSearch().types(types), query);
	}

	public static ResourceSearch buildResourceSearch(String query, String... types) {
		return buildSearch(new ResourceSearch().types(types), query);
	}

	public static ActivitySearch buildActivitySearch(String query, String... types) {
		return buildSearch(new ActivitySearch().types(types), query);
	}

	@SuppressWarnings("unchecked")
	private static <T extends StrolchRootElement, U extends StrolchSearch<T>> U buildSearch(U search, String query) {
		query = trimOrEmpty(query);

		if (query.isEmpty())
			return search;

		SearchExpression<T> se = null;

		String[] parts = query.split(" ");
		for (String part : parts) {

			if (!part.startsWith("param:")) {

				if (se == null)
					se = (SearchExpression<T>) id(containsIgnoreCase(part)).or(name(containsIgnoreCase(part)));
				else
					se = se.or(id(containsIgnoreCase(part))).or(name(containsIgnoreCase(part)));

			} else {
				String[] paramParts = part.split(":");
				if (paramParts.length != 4) {

					if (se == null)
						se = (SearchExpression<T>) id(containsIgnoreCase(part)).or(name(containsIgnoreCase(part)));
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

		search = (U) search.where(se);
		return search;
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
