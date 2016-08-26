package li.strolch.rest.visitor;

import com.google.gson.JsonObject;

import li.strolch.model.Resource;
import li.strolch.model.visitor.ResourceVisitor;

public class ResourceToFlatJsonVisitor extends ToFlatJsonVisitor implements ResourceVisitor<JsonObject> {

	@Override
	public JsonObject visit(Resource element) {
		return toJson(element);
	}
}
