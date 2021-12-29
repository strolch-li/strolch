/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.model.xml;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.activity.Activity;

/**
 * {@link StrolchElementListener} to store the {@link StrolchRootElement} in {@link Map} objects when parsing an object
 * model from an external source
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class SimpleStrolchElementListener implements StrolchElementListener {

	private final Map<String, Resource> resources = new HashMap<>();
	private final Map<String, Order> orders = new HashMap<>();
	private final Map<String, Activity> activities = new HashMap<>();

	@Override
	public void notifyResource(Resource resource) {
		this.resources.put(resource.getId(), resource);
	}

	@Override
	public void notifyOrder(Order order) {
		this.orders.put(order.getId(), order);
	}

	@Override
	public void notifyActivity(Activity activity) {
		this.activities.put(activity.getId(), activity);
	}

	public Resource getResource(String id) {
		return this.resources.get(id);
	}

	public Order getOrder(String id) {
		return this.orders.get(id);
	}

	public Activity getActivity(String id) {
		return this.activities.get(id);
	}

	public List<Resource> getResources() {
		return new ArrayList<>(this.resources.values());
	}

	public List<Order> getOrders() {
		return new ArrayList<>(this.orders.values());
	}

	public List<Activity> getActivities() {
		return new ArrayList<>(this.activities.values());
	}

	public List<StrolchRootElement> getElements() {
		List<StrolchRootElement> elements = new ArrayList<>();
		elements.addAll(getResources());
		elements.addAll(getOrders());
		elements.addAll(getActivities());
		return elements;
	}

	public Stream<StrolchRootElement> streamElements() {
		return Stream.concat(this.resources.values().stream(),
				Stream.concat(this.orders.values().stream(), this.activities.values().stream()));
	}
}
