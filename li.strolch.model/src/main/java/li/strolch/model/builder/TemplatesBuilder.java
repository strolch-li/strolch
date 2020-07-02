package li.strolch.model.builder;

import static java.util.stream.Collectors.toList;
import static li.strolch.model.StrolchModelConstants.TEMPLATE;

import java.util.List;

import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.utils.collections.MapOfMaps;

public class TemplatesBuilder {

	private final MapOfMaps<String, String, RootElementBuilder<?>> builders;

	public TemplatesBuilder() {
		this.builders = new MapOfMaps<>();
	}

	public ResourceBuilder resource(String name, String type) {
		ResourceBuilder builder = new ResourceBuilder(this, type, name, TEMPLATE);
		this.builders.addElement(Tags.Json.RESOURCE, type, builder);
		return builder;
	}

	public OrderBuilder order(String name, String type) {
		OrderBuilder builder = new OrderBuilder(this, type, name, TEMPLATE);
		this.builders.addElement(Tags.Json.ORDER, type, builder);
		return builder;
	}

	public ActivityBuilder activity(String name, String type, TimeOrdering timeOrdering) {
		ActivityBuilder builder = new ActivityBuilder(this, type, name, TEMPLATE, timeOrdering);
		this.builders.addElement(Tags.Json.ACTIVITY, type, builder);
		return builder;
	}

	public List<StrolchRootElement> buildTemplates() {
		return this.builders.streamValues().map(RootElementBuilder::build).collect(toList());
	}
}
