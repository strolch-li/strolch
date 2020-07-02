package li.strolch.model.builder;

import li.strolch.model.PolicyContainer;
import li.strolch.model.activity.Action;

public class ActionBuilder extends PolicyContainerBuilder<ActionBuilder> implements ActivityElementBuilder {

	private final ActivityBuilder builder;

	private String resourceId;
	private String resourceType;

	public ActionBuilder(ActivityBuilder builder, String id, String name, String type) {
		super(id, name, type);
		this.builder = builder;
	}

	public ActivityBuilder endAction() {
		return this.builder;
	}

	public ActionBuilder resource(String type, String id) {
		this.resourceType = type;
		this.resourceId = id;
		return this;
	}

	@Override
	public Action build() {
		Action action = new Action(getId(), getName(), getType());
		action.setResourceId(this.resourceId);
		action.setResourceType(this.resourceType);
		build((PolicyContainer) action);
		return action;
	}
}
