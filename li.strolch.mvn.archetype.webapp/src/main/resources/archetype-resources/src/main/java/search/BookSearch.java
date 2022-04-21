package ${package}.search;

import static ${package}.model.Constants.*;
import static li.strolch.utils.helper.StringHelper.isEmpty;

import li.strolch.search.ResourceSearch;

public class BookSearch extends ResourceSearch {

	public BookSearch() {
		types(TYPE_BOOK);
	}

	public BookSearch stringQuery(String value) {
		if (isEmpty(value))
			return this;

		// split by spaces
		value = value.trim();
		String[] values = value.split(" ");

		// add where clauses for id, name and description
		where(id().containsIgnoreCase(values) //
				.or(name().containsIgnoreCase(values)) //
				.or(param(BAG_PARAMETERS, PARAM_TITLE).containsIgnoreCase(values)));

		return this;
	}
}
