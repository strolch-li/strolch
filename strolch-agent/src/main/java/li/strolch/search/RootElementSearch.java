package li.strolch.search;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;

import java.util.stream.Stream;

/**
 * Performs a search for any kind of root element, allowing to mix {@link Resource}, {@link Order} and {@link Activity}
 * in the result
 */
public class RootElementSearch extends StrolchSearch<StrolchRootElement> {

	private SearchNavigator<StrolchRootElement> navigator;

	@Override
	protected SearchNavigator<StrolchRootElement> getNavigator() {
		return this.navigator;
	}

	@Override
	public RootElementSearch types(String... types) {
		this.navigator = tx -> {

			Stream<Resource> resources = tx.streamResources(types);
			Stream<Activity> activities = tx.streamActivities(types);
			Stream<Order> orders = tx.streamOrders(types);

			return Stream.concat(resources, Stream.concat(activities, orders));
		};

		return this;
	}

	@Override
	public RootElementSearchResult<StrolchRootElement> search(StrolchTransaction tx) {
		return new RootElementSearchResult<>(prepareSearch(tx));
	}
}
