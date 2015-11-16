package li.strolch.model.json;

import com.google.gson.Gson;
import com.google.gson.JsonObject;

import li.strolch.model.Resource;
import li.strolch.model.ResourceVisitor;

public class ResourceToJsonVisitor extends StrolchElementToJsonVisitor implements ResourceVisitor<JsonObject> {

	@Override
	public JsonObject visit(Resource element) {

		JsonObject rootJ = toJson(element);

		return rootJ;
	}

	public static String toJsonString(Resource element) {
		ResourceToJsonVisitor visitor = new ResourceToJsonVisitor();
		JsonObject jsonObject = visitor.visit(element);
		String entity = new Gson().toJson(jsonObject);
		return entity;
	}
}
