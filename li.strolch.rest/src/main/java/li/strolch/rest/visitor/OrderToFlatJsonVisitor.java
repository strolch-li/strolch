package li.strolch.rest.visitor;

import com.google.gson.JsonObject;

import li.strolch.model.Order;
import li.strolch.model.visitor.OrderVisitor;

public class OrderToFlatJsonVisitor extends ToFlatJsonVisitor implements OrderVisitor<JsonObject> {

	@Override
	public JsonObject visit(Order element) {
		return toJson(element);
	}
}
