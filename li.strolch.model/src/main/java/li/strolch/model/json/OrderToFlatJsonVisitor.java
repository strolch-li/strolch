package li.strolch.model.json;

import java.util.function.BiConsumer;

import com.google.gson.JsonObject;

import li.strolch.model.Order;
import li.strolch.model.visitor.OrderVisitor;

public class OrderToFlatJsonVisitor extends ToFlatJsonVisitor<Order> implements OrderVisitor<JsonObject> {

	public OrderToFlatJsonVisitor() {
		super();
	}

	public OrderToFlatJsonVisitor(boolean withVersion) {
		super(withVersion);
	}

	@Override
	public JsonObject visit(Order element) {
		return toJson(element);
	}

	@Override
	public OrderToFlatJsonVisitor withoutElementName() {
		super.withoutElementName();
		return this;
	}

	@Override
	public OrderToFlatJsonVisitor setHook(BiConsumer<Order, JsonObject> hook) {
		super.setHook(hook);
		return this;
	}

	@Override
	public OrderToFlatJsonVisitor ignoreBag(String bagId) {
		super.ignoreBag(bagId);
		return this;
	}

	@Override
	public OrderToFlatJsonVisitor ignoreParameter(String bagId, String paramId) {
		super.ignoreParameter(bagId, paramId);
		return this;
	}
}
