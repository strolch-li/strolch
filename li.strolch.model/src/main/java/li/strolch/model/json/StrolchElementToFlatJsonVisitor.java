package li.strolch.model.json;

import com.google.gson.JsonObject;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.model.visitor.StrolchRootElementVisitor;

public class StrolchElementToFlatJsonVisitor implements StrolchRootElementVisitor<JsonObject> {

	@Override
	public JsonObject visitOrder(Order order) {
		return new OrderToFlatJsonVisitor().toJson(order);
	}

	@Override
	public JsonObject visitResource(Resource resource) {
		return new ResourceToFlatJsonVisitor().toJson(resource);
	}

	@Override
	public JsonObject visitActivity(Activity activity) {
		return new ActivityToFlatJsonVisitor().toJson(activity);
	}
}
