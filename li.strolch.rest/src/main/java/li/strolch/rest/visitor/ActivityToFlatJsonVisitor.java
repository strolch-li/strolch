package li.strolch.rest.visitor;

import com.google.gson.JsonObject;

import li.strolch.model.activity.Activity;
import li.strolch.model.visitor.ActivityVisitor;

public class ActivityToFlatJsonVisitor extends ToFlatJsonVisitor<Activity> implements ActivityVisitor<JsonObject> {

	@Override
	public JsonObject visit(Activity element) {
		return toJson(element);
	}
}
