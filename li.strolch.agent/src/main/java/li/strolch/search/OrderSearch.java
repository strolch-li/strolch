package li.strolch.search;

import li.strolch.model.Order;

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
}
