package li.strolch.model.builder;

import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.utils.dbc.DBC;

import java.util.ArrayList;
import java.util.List;

import static li.strolch.model.builder.BuilderHelper.buildParamName;

public class ActivityBuilder extends RootElementBuilder<ActivityBuilder> implements ActivityElementBuilder {

	private final StrolchElementBuilder builder;
	private final ActivityBuilder parentBuilder;
	private final TimeOrdering timeOrdering;

	private final List<ActivityElementBuilder> builders;

	public ActivityBuilder(String id, String type, TimeOrdering timeOrdering) {
		this(id, buildParamName(id), type, timeOrdering);
	}

	public ActivityBuilder(String id, String name, String type, TimeOrdering timeOrdering) {
		super(id, name, type);
		this.builder = null;
		this.parentBuilder = null;
		this.timeOrdering = timeOrdering;
		this.builders = new ArrayList<>();
	}

	public ActivityBuilder(StrolchElementBuilder builder, String id, String type, TimeOrdering timeOrdering) {
		this(builder, id, buildParamName(id), type, timeOrdering);
	}

	public ActivityBuilder(StrolchElementBuilder builder, String id, String name, String type,
			TimeOrdering timeOrdering) {
		super(id, name, type);
		this.builder = builder;
		this.parentBuilder = null;
		this.timeOrdering = timeOrdering;
		this.builders = new ArrayList<>();
	}

	public ActivityBuilder(StrolchElementBuilder builder, ActivityBuilder parentBuilder, String id, String type,
			TimeOrdering timeOrdering) {
		this(builder, parentBuilder, id, buildParamName(id), type, timeOrdering);
	}

	public ActivityBuilder(StrolchElementBuilder builder, ActivityBuilder parentBuilder, String id, String name,
			String type, TimeOrdering timeOrdering) {
		super(id, name, type);
		this.builder = builder;
		this.parentBuilder = parentBuilder;
		this.timeOrdering = timeOrdering;
		this.builders = new ArrayList<>();
	}

	public ActivityBuilder subActivity(String id, String type, TimeOrdering timeOrdering) {
		return subActivity(id, buildParamName(id), type, timeOrdering);
	}

	public ActivityBuilder subActivity(String id, String name, String type, TimeOrdering timeOrdering) {
		ActivityBuilder builder = new ActivityBuilder(this.builder, this, id, name, type, timeOrdering);
		this.builders.add(builder);
		return builder;
	}

	public ActionBuilder action(String id, String type) {
		return action(id, buildParamName(id), type);
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

	public StrolchElementBuilder endActivity() {
		DBC.PRE.assertNotNull("Can not end, as not part of a builder context!", this.builder);
		return this.builder;
	}

	@Override
	public Activity build(String id) {
		Activity activity = new Activity(id, getName(), getType(), this.timeOrdering);
		return applyBuilder(activity);
	}

	@Override
	public Activity build(String id, String name) {
		Activity activity = new Activity(id, name, getType(), this.timeOrdering);
		return applyBuilder(activity);
	}

	@Override
	public Activity build() {
		Activity activity = new Activity(getId(), getName(), getType(), this.timeOrdering);
		return applyBuilder(activity);
	}

	private Activity applyBuilder(Activity activity) {
		super.applyRootElement(activity);
		this.builders.forEach(b -> activity.addElement(b.build()));
		return activity;
	}
}
