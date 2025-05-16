/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.utils.collections.MapOfMaps;

import java.util.Optional;
import java.util.stream.Stream;

public class StrolchElementToMapListener implements StrolchElementListener {

	private final MapOfMaps<String, String, Resource> resourceMap = new MapOfMaps<>();
	private final MapOfMaps<String, String, Order> orderMap = new MapOfMaps<>();
	private final MapOfMaps<String, String, Activity> activityMap = new MapOfMaps<>();

	public MapOfMaps<String, String, Resource> getResourceMap() {
		return this.resourceMap;
	}

	public MapOfMaps<String, String, Order> getOrderMap() {
		return this.orderMap;
	}

	public MapOfMaps<String, String, Activity> getActivityMap() {
		return this.activityMap;
	}

	public Optional<Resource> getResource(String type, String id) {
		return Optional.ofNullable(this.resourceMap.getElement(type, id));
	}

	public Optional<Order> getOrder(String type, String id) {
		return Optional.ofNullable(this.orderMap.getElement(type, id));
	}

	public Optional<Activity> getActivity(String type, String id) {
		return Optional.ofNullable(this.activityMap.getElement(type, id));
	}

	public Stream<Resource> resourceStream(String type) {
		return this.resourceMap.getAllElements(type).stream();
	}

	public Stream<Order> orderStream(String type) {
		return this.orderMap.getAllElements(type).stream();
	}

	public Stream<Activity> activityStream(String type) {
		return this.activityMap.getAllElements(type).stream();
	}

	@Override
	public void notifyResource(Resource resource) {
		this.resourceMap.addElement(resource.getType(), resource.getId(), resource);
	}

	@Override
	public void notifyOrder(Order order) {
		this.orderMap.addElement(order.getType(), order.getId(), order);
	}

	@Override
	public void notifyActivity(Activity activity) {
		this.activityMap.addElement(activity.getType(), activity.getId(), activity);
	}
}
