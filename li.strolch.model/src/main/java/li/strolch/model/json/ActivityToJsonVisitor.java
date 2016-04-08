package li.strolch.model.json;

import com.google.gson.Gson;
import com.google.gson.JsonObject;

import li.strolch.model.ActivityVisitor;
import li.strolch.model.activity.Activity;

public class ActivityToJsonVisitor extends StrolchElementToJsonVisitor implements ActivityVisitor<JsonObject> {

	@Override
	public JsonObject visit(Activity element) {

		JsonObject rootJ = toJson(element);

		return rootJ;
	}

	public static String toJsonString(Activity element) {
		ActivityToJsonVisitor visitor = new ActivityToJsonVisitor();
		JsonObject jsonObject = visitor.visit(element);
		String entity = new Gson().toJson(jsonObject);
		return entity;
	}
}
