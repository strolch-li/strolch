package li.strolch.model.builder;

import li.strolch.model.Order;
import li.strolch.model.State;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.iso8601.ISO8601;

import static li.strolch.model.builder.BuilderHelper.buildParamName;

public class OrderBuilder extends RootElementBuilder<OrderBuilder> {

	private final StrolchElementBuilder builder;

	public OrderBuilder(String id, String type) {
		this(id, buildParamName(id), type);
	}

	public OrderBuilder(String id, String name, String type) {
		super(id, name, type);
		this.builder = null;
	}

	public OrderBuilder(StrolchElementBuilder builder, String id, String type) {
		this(builder, id, buildParamName(id), type);
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
	public Order build(String id) {
		Order order = new Order(id, getName(), getType());
		return applyBuilder(order);
	}

	@Override
	public Order build(String id, String name) {
		Order order = new Order(id, name, getType());
		return applyBuilder(order);
	}

	@Override
	public Order build() {
		Order order = new Order(getId(), getName(), getType());
		return applyBuilder(order);
	}

	private Order applyBuilder(Order order) {
		super.applyRootElement(order);
		order.setDate(ISO8601.EMPTY_VALUE_ZONED_DATE);
		order.setState(State.CREATED);
		return order;
	}
}
