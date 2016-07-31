package li.strolch.model.json;

import com.google.gson.Gson;
import com.google.gson.JsonObject;

import li.strolch.model.Order;
import li.strolch.model.OrderVisitor;

public class OrderToJsonVisitor extends StrolchElementToJsonVisitor implements OrderVisitor<JsonObject> {

	private JsonObject jsonObject;

	public JsonObject getJsonObject() {
		return this.jsonObject;
	}

	@Override
	public JsonObject visit(Order element) {
		this.jsonObject = toJson(element);
		return this.jsonObject;
	}

	public static String toJsonString(Order element) {
		OrderToJsonVisitor visitor = new OrderToJsonVisitor();
		JsonObject jsonObject = visitor.visit(element);
		String entity = new Gson().toJson(jsonObject);
		return entity;
	}
}
