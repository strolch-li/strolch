package li.strolch.search;

import li.strolch.model.Resource;

/**
 * Performs a search for {@link Resource} elements
 */
public class ResourceSearch extends StrolchSearch<Resource> {

	private SearchNavigator<Resource> navigator;

	@Override
	protected SearchNavigator<Resource> getNavigator() {
		return this.navigator;
	}

	@Override
	public ResourceSearch types(String... types) {
		this.navigator = tx -> tx.streamResources(types);
		return this;
	}

	@Override
	public ResourceSearch where(SearchExpression<Resource> expression) {
		super.where(expression);
		return this;
	}
}
