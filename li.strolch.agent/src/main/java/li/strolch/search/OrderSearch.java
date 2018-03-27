package li.strolch.search;

import li.strolch.model.Order;

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
}
