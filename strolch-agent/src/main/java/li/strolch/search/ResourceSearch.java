package li.strolch.search;

import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;

import java.util.stream.Stream;

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
		this.navigator = tx -> {
			Stream<Resource> cachedStream = tx.streamCachedResources(types);
			Stream<Resource> stream = tx
					.streamResources(types)
					.filter(e -> !tx.isResourceCached(e.getType(), e.getId()));
			return Stream.concat(cachedStream, stream);
		};
		return this;
	}

	@Override
	public ResourceSearch where(SearchExpression<Resource> expression) {
		super.where(expression);
		return this;
	}

	@Override
	public ResourceSearch internal() {
		super.internal();
		return this;
	}

	@Override
	public ResourceSearchResult search(StrolchTransaction tx) {
		return new ResourceSearchResult(prepareSearch(tx));
	}
}
