package li.strolch.search;

import li.strolch.model.Order;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;

import java.util.stream.Stream;

import static li.strolch.model.StrolchModelConstants.PolicyConstants.PARAM_ORDER;

/**
 * Performs a search for {@link Activity} elements
 */
public class ActivitySearch extends StrolchSearch<Activity, ActivitySearchResult> {

	private SearchNavigator<Activity> navigator;

	@Override
	protected SearchNavigator<Activity> getNavigator() {
		return this.navigator;
	}

	@Override
	public ActivitySearch types(String... types) {
		this.navigator = tx -> {
			Stream<Activity> cachedStream = tx.streamCachedActivities(types);
			Stream<Activity> stream = tx
					.streamActivities(types)
					.filter(e -> !tx.isActivityCached(e.getType(), e.getId()));
			return Stream.concat(cachedStream, stream);
		};
		return this;
	}

	public ActivitySearch forOrder(Order order) {
		types(order.getType());
		where(relation(PARAM_ORDER).isEqualTo(order.getId()));
		return this;
	}

	@Override
	public ActivitySearch where(SearchExpression<Activity> expression) {
		super.where(expression);
		return this;
	}

	@Override
	public ActivitySearch internal() {
		super.internal();
		return this;
	}

	@Override
	protected ActivitySearchResult evaluateResult(Stream<Activity> stream) {
		return new ActivitySearchResult(stream);
	}
}
