package ${package}.search;

import static ${package}.model.Constants.*;
import static li.strolch.utils.helper.StringHelper.isEmpty;

import li.strolch.search.ResourceSearch;

public class LocationSearch extends ResourceSearch {

	public LocationSearch() {
		types(TYPE_LOCATION);
	}

	public LocationSearch stringQuery(String value) {
		if (isEmpty(value))
			return this;

		// split by spaces
		value = value.trim();
		String[] values = value.split(" ");

		// add where clauses for id, name and description
		where(id().containsIgnoreCase(values) //
				.or(name().containsIgnoreCase(values)) //
				.or(param(PARAM_CITY).containsIgnoreCase(values)));

		return this;
	}
}
