/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package li.strolch.model.builder;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.utils.helper.StringHelper;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Stream.concat;
import static li.strolch.model.StrolchModelConstants.TEMPLATE;

public class StrolchElementBuilder {

	private final Map<String, ResourceBuilder> resourceBuilders;
	private final Map<String, OrderBuilder> orderBuilders;
	private final Map<String, ActivityBuilder> activityBuilders;

	public StrolchElementBuilder() {
		this.resourceBuilders = new HashMap<>();
		this.orderBuilders = new HashMap<>();
		this.activityBuilders = new HashMap<>();
	}

	public ResourceBuilder resourceTemplate(String name, String type) {
		ResourceBuilder builder = new ResourceBuilder(this, type, name, TEMPLATE);
		this.resourceBuilders.put(type, builder);
		return builder;
	}

	public OrderBuilder orderTemplate(String name, String type) {
		OrderBuilder builder = new OrderBuilder(this, type, name, TEMPLATE);
		this.orderBuilders.put(type, builder);
		return builder;
	}

	public ActivityBuilder activityTemplate(String name, String type, TimeOrdering timeOrdering) {
		ActivityBuilder builder = new ActivityBuilder(this, type, name, TEMPLATE, timeOrdering);
		this.activityBuilders.put(type, builder);
		return builder;
	}

	public List<Resource> buildResourceTemplates() {
		return this.resourceBuilders.values().stream() //
				.map(ResourceBuilder::build).collect(toList());
	}

	public List<Order> buildOrderTemplates() {
		return this.orderBuilders.values().stream() //
				.map(OrderBuilder::build).collect(toList());
	}

	public List<Activity> buildActivityTemplates() {
		return this.activityBuilders.values().stream() //
				.map(ActivityBuilder::build).collect(toList());
	}

	public List<StrolchRootElement> buildTemplates() {
		return concat(concat(this.resourceBuilders.values().stream(), //
						this.orderBuilders.values().stream()), //
				this.activityBuilders.values().stream()) //
				.map(RootElementBuilder::build).collect(toList());
	}

	public Resource newResource(String type, String newName) {
		ResourceBuilder builder = this.resourceBuilders.get(type);
		if (builder == null)
			throw new IllegalArgumentException("No resource template defined for type " + type);
		return setInitialFields(type, newName, builder.build());
	}

	public Order newOrder(String type, String newName) {
		OrderBuilder builder = this.orderBuilders.get(type);
		if (builder == null)
			throw new IllegalArgumentException("No resource template defined for type " + type);
		return setInitialFields(type, newName, builder.build());
	}

	public Activity newActivity(String type, String newName) {
		ActivityBuilder builder = this.activityBuilders.get(type);
		if (builder == null)
			throw new IllegalArgumentException("No resource template defined for type " + type);
		return setInitialFields(type, newName, builder.build());
	}

	private <T extends StrolchRootElement> T setInitialFields(String type, String newName, T element) {
		element.setId(StringHelper.getUniqueId());
		element.setName(newName);
		element.setType(type);
		return element;
	}
}
