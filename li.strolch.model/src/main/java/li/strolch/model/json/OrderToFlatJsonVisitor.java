package li.strolch.model.json;

import java.util.function.BiConsumer;

import com.google.gson.JsonObject;

import li.strolch.model.Order;
import li.strolch.model.Tags;
import li.strolch.model.visitor.OrderVisitor;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

public class OrderToFlatJsonVisitor extends ToFlatJsonVisitor<Order> implements OrderVisitor<JsonObject> {

	public OrderToFlatJsonVisitor() {
		super();
	}

	@Override
	public OrderToFlatJsonVisitor withVersion() {
		super.withVersion();
		return this;
	}

	@Override
	public OrderToFlatJsonVisitor withoutElementName() {
		super.withoutElementName();
		return this;
	}

	@Override
	public OrderToFlatJsonVisitor hook(BiConsumer<Order, JsonObject> hook) {
		super.hook(hook);
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

	@Override
	public JsonObject visit(Order element) {
		JsonObject jsonObject = toJson(element);

		jsonObject.addProperty(Tags.Json.DATE, ISO8601FormatFactory.getInstance().formatDate(element.getDate()));
		jsonObject.addProperty(Tags.Json.STATE, element.getState().getName());

		return jsonObject;
	}
}
