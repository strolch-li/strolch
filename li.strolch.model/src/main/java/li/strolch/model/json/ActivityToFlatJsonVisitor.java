package li.strolch.model.json;

import java.util.function.BiConsumer;

import com.google.gson.JsonObject;

import li.strolch.model.Tags;
import li.strolch.model.activity.Activity;
import li.strolch.model.visitor.ActivityVisitor;

public class ActivityToFlatJsonVisitor extends ToFlatJsonVisitor<Activity> implements ActivityVisitor<JsonObject> {

	public ActivityToFlatJsonVisitor() {
		super();
	}

	@Override
	public ActivityToFlatJsonVisitor withVersion() {
		super.withVersion();
		return this;
	}

	@Override
	public ActivityToFlatJsonVisitor withoutElementName() {
		super.withoutElementName();
		return this;
	}

	@Override
	public ActivityToFlatJsonVisitor hook(BiConsumer<Activity, JsonObject> hook) {
		super.hook(hook);
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

	@Override
	public JsonObject visit(Activity element) {
		JsonObject jsonObject = toJson(element);
		jsonObject.addProperty(Tags.Json.STATE, element.getState().getName());
		return jsonObject;
	}
}
