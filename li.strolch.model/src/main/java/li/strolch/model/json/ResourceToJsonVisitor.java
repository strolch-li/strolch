package li.strolch.model.json;

import com.google.gson.Gson;
import com.google.gson.JsonObject;

import li.strolch.model.Resource;
import li.strolch.model.ResourceVisitor;

public class ResourceToJsonVisitor extends StrolchElementToJsonVisitor implements ResourceVisitor<JsonObject> {

	private JsonObject jsonObject;

	public JsonObject getJsonObject() {
		return this.jsonObject;
	}

	@Override
	public JsonObject visit(Resource element) {
		this.jsonObject = toJson(element);
		return this.jsonObject;
	}

	public static String toJsonString(Resource element) {
		ResourceToJsonVisitor visitor = new ResourceToJsonVisitor();
		JsonObject jsonObject = visitor.visit(element);
		String entity = new Gson().toJson(jsonObject);
		return entity;
	}
}
