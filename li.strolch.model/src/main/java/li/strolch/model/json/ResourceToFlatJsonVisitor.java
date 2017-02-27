package li.strolch.model.json;

import java.util.function.BiConsumer;

import com.google.gson.JsonObject;

import li.strolch.model.Resource;
import li.strolch.model.visitor.ResourceVisitor;

public class ResourceToFlatJsonVisitor extends ToFlatJsonVisitor<Resource> implements ResourceVisitor<JsonObject> {

	@Override
	public JsonObject visit(Resource element) {
		return toJson(element);
	}

	@Override
	public ResourceToFlatJsonVisitor withoutElementName() {
		super.withoutElementName();
		return this;
	}

	@Override
	public ResourceToFlatJsonVisitor setHook(BiConsumer<Resource, JsonObject> hook) {
		super.setHook(hook);
		return this;
	}

	@Override
	public ResourceToFlatJsonVisitor ignoreBag(String bagId) {
		super.ignoreBag(bagId);
		return this;
	}

	@Override
	public ResourceToFlatJsonVisitor ignoreParameter(String bagId, String paramId) {
		super.ignoreParameter(bagId, paramId);
		return this;
	}
}
