package li.strolch.model.builder;

import li.strolch.model.Order;
import li.strolch.model.State;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.iso8601.ISO8601;

public class OrderBuilder extends RootElementBuilder<OrderBuilder> {

	private final StrolchElementBuilder builder;

	public OrderBuilder(String id, String name, String type) {
		super(id, name, type);
		this.builder = null;
	}

	public OrderBuilder(StrolchElementBuilder builder, String id, String name, String type) {
		super(id, name, type);
		this.builder = builder;
	}

	public StrolchElementBuilder endOrder() {
		DBC.PRE.assertNotNull("Can not end, as not part of a builder context!", this.builder);
		return this.builder;
	}

	@Override
	public Order build() {
		Order order = new Order(getId(), getName(), getType());
		super.applyRootElement(order);
		order.setDate(ISO8601.EMPTY_VALUE_ZONED_DATE);
		order.setState(State.CREATED);
		return order;
	}
}
