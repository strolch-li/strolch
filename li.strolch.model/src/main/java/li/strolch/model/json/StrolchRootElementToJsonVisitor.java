package li.strolch.model.json;

import java.util.function.BiConsumer;

import com.google.gson.JsonObject;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.visitor.ActivityVisitor;
import li.strolch.model.visitor.OrderVisitor;
import li.strolch.model.visitor.ResourceVisitor;
import li.strolch.model.visitor.StrolchRootElementVisitor;

public class StrolchRootElementToJsonVisitor implements StrolchRootElementVisitor<JsonObject> {

	private StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor();

	@Override
	public JsonObject visitOrder(Order order) {
		return this.visitor.visitOrder(order).getAsJsonObject();
	}

	@Override
	public JsonObject visitResource(Resource resource) {
		return this.visitor.visitResource(resource).getAsJsonObject();
	}

	@Override
	public JsonObject visitActivity(Activity activity) {
		return this.visitor.visitActivity(activity).getAsJsonObject();
	}

	@Override
	public JsonObject visitAction(Action action) {
		return this.visitor.visitAction(action).getAsJsonObject();
	}

	public OrderVisitor<JsonObject> asOrderVisitor() {
		return this::visitOrder;
	}

	public ResourceVisitor<JsonObject> asResourceVisitor() {
		return this::visitResource;
	}

	public ActivityVisitor<JsonObject> asActivityVisitor() {
		return this::visitActivity;
	}

	public StrolchRootElementToJsonVisitor withVersion() {
		this.visitor.withVersion();
		return this;
	}

	public StrolchRootElementToJsonVisitor withoutVersion() {
		this.visitor.withoutVersion();
		return this;
	}

	public StrolchRootElementToJsonVisitor withoutElementName() {
		this.visitor.withoutElementName();
		return this;
	}

	public StrolchRootElementToJsonVisitor withElementName() {
		this.visitor.withElementName();
		return this;
	}

	public StrolchRootElementToJsonVisitor withoutPolicies() {
		this.visitor.withoutPolicies();
		return this;
	}

	public StrolchRootElementToJsonVisitor withPolicies() {
		this.visitor.withPolicies();
		return this;
	}

	public StrolchRootElementToJsonVisitor activityDepth(int depth) {
		this.visitor.activityDepth(depth);
		return this;
	}

	public StrolchRootElementToJsonVisitor flat() {
		this.visitor.flat();
		return this;
	}

	public StrolchRootElementToJsonVisitor ignoreBag(String bagId) {
		this.visitor.ignoreBag(bagId);
		return this;
	}

	public StrolchRootElementToJsonVisitor ignoreParameter(String bagId, String paramId) {
		this.visitor.ignoreParameter(bagId, paramId);
		return this;
	}

	public StrolchRootElementToJsonVisitor ignoreTimeState(String timeStateId) {
		this.visitor.ignoreTimeState(timeStateId);
		return this;
	}

	public StrolchRootElementToJsonVisitor ignoreBagByType(String type) {
		this.visitor.ignoreBagByType(type);
		return this;
	}

	public StrolchRootElementToJsonVisitor resourceHook(BiConsumer<Resource, JsonObject> hook) {
		this.visitor.resourceHook(hook);
		return this;
	}

	public StrolchRootElementToJsonVisitor orderHook(BiConsumer<Order, JsonObject> hook) {
		this.visitor.orderHook(hook);
		return this;
	}

	public StrolchRootElementToJsonVisitor activityHook(BiConsumer<Activity, JsonObject> hook) {
		this.visitor.activityHook(hook);
		return this;
	}

	public StrolchRootElementToJsonVisitor actionHook(BiConsumer<Action, JsonObject> hook) {
		this.visitor.actionHook(hook);
		return this;
	}
}
