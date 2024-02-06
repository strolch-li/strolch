package li.strolch.search;

import li.strolch.model.Order;
import li.strolch.model.State;
import li.strolch.persistence.api.StrolchTransaction;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;

/**
 * Performs a search of {@link Order} elements
 */
public class OrderSearch extends StrolchSearch<Order> {

	private SearchNavigator<Order> navigator;

	@Override
	protected SearchNavigator<Order> getNavigator() {
		return this.navigator;
	}

	@Override
	public OrderSearch types(String... types) {
		this.navigator = tx -> {
			Stream<Order> cachedStream = tx.streamCachedOrders(types);
			Stream<Order> stream = tx.streamOrders(types).filter(e -> !tx.isOrderCached(e.getType(), e.getId()));
			return Stream.concat(cachedStream, stream);
		};
		return this;
	}

	@Override
	public OrderSearch where(SearchExpression<Order> expression) {
		super.where(expression);
		return this;
	}

	public OrderSearch stateIsIn(String states) {
		if (states == null)
			return this;
		String trim = states.trim();
		if (trim.isEmpty())
			return this;

		List<State> stateList = Arrays.stream(trim.split(",")) //
				.map(e -> State.parse(e.trim())) //
				.collect(toList());
		super.where(state().isIn(stateList));
		return this;
	}

	public OrderSearch stateIsIn(State... states) {
		super.where(state().isIn(states));
		return this;
	}

	public OrderSearch withState(State state) {
		super.where(state().isEqualTo(state));
		return this;
	}

	@Override
	public OrderSearch internal() {
		super.internal();
		return this;
	}

	@Override
	public OrderSearchResult search(StrolchTransaction tx) {
		return new OrderSearchResult(prepareSearch(tx));
	}
}
