package li.strolch.model.builder;

import java.util.ArrayList;
import java.util.List;

import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.utils.dbc.DBC;

public class ActivityBuilder extends RootElementBuilder<ActivityBuilder> implements ActivityElementBuilder {

	private final TemplatesBuilder templatesBuilder;
	private final ActivityBuilder parentBuilder;
	private final TimeOrdering timeOrdering;

	private final List<ActivityElementBuilder> builders;

	public ActivityBuilder(TemplatesBuilder builder, String id, String name, String type, TimeOrdering timeOrdering) {
		super(id, name, type);
		this.templatesBuilder = builder;
		this.parentBuilder = null;
		this.timeOrdering = timeOrdering;
		this.builders = new ArrayList<>();
	}

	public ActivityBuilder(TemplatesBuilder builder, ActivityBuilder parentBuilder, String id, String name, String type,
			TimeOrdering timeOrdering) {
		super(id, name, type);
		this.templatesBuilder = builder;
		this.parentBuilder = parentBuilder;
		this.timeOrdering = timeOrdering;
		this.builders = new ArrayList<>();
	}

	public ActivityBuilder subActivity(String id, String name, String type, TimeOrdering timeOrdering) {
		ActivityBuilder builder = new ActivityBuilder(this.templatesBuilder, this, id, name, type, timeOrdering);
		this.builders.add(builder);
		return builder;
	}

	public ActionBuilder action(String id, String name, String type) {
		ActionBuilder builder = new ActionBuilder(this, id, name, type);
		this.builders.add(builder);
		return builder;
	}

	public ActivityBuilder endSubActivity() {
		DBC.PRE.assertNotNull("all sub activities already closed", this.parentBuilder);
		return this.parentBuilder;
	}

	public TemplatesBuilder endActivity() {
		return this.templatesBuilder;
	}

	@Override
	public Activity build() {
		Activity activity = new Activity(getId(), getName(), getType(), this.timeOrdering);
		super.build(activity);

		this.builders.forEach(b -> activity.addElement(b.build()));

		return activity;
	}
}
