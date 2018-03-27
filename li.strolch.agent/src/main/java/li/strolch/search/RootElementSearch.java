package li.strolch.search;

import java.util.stream.Stream;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.activity.Activity;

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
}
