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
import li.strolch.model.StrolchRootElement;
import li.strolch.model.activity.Activity;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

public class StrolchElementListenerToListListener implements StrolchElementListener {

	private final List<StrolchRootElement> elements;

	public StrolchElementListenerToListListener() {
		this.elements = new ArrayList<>();
	}

	@Override
	public void notifyResource(Resource resource) {
		this.elements.add(resource);
	}

	@Override
	public void notifyOrder(Order order) {
		this.elements.add(order);
	}

	@Override
	public void notifyActivity(Activity activity) {
		this.elements.add(activity);
	}

	public Stream<StrolchRootElement> streamElements() {
		return elements.stream();
	}

	public List<StrolchRootElement> getElements() {
		return elements;
	}
}
