package li.strolch.model.json;

import java.util.function.BiConsumer;

import com.google.gson.JsonObject;

import li.strolch.model.activity.Activity;
import li.strolch.model.visitor.ActivityVisitor;

public class ActivityToFlatJsonVisitor extends ToFlatJsonVisitor<Activity> implements ActivityVisitor<JsonObject> {

	@Override
	public JsonObject visit(Activity element) {
		return toJson(element);
	}

	@Override
	public ActivityToFlatJsonVisitor withoutElementName() {
		super.withoutElementName();
		return this;
	}

	@Override
	public ActivityToFlatJsonVisitor setHook(BiConsumer<Activity, JsonObject> hook) {
		super.setHook(hook);
		return this;
	}

	@Override
	public ActivityToFlatJsonVisitor ignoreBag(String bagId) {
		super.ignoreBag(bagId);
		return this;
	}

	@Override
	public ActivityToFlatJsonVisitor ignoreParameter(String bagId, String paramId) {
		super.ignoreParameter(bagId, paramId);
		return this;
	}
}
