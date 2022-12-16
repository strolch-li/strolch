package li.strolch.search;

import static java.util.stream.Collectors.toList;

import java.util.Arrays;
import java.util.List;

import li.strolch.model.Order;
import li.strolch.model.State;

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
		this.navigator = tx -> tx.streamOrders(types);
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
}
