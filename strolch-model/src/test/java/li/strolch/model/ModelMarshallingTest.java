/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.model;

import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public abstract class ModelMarshallingTest {

	@Test
	public void shouldFormatAndParseOrder() throws Exception {
		Order order = ModelGenerator.createOrder("@1", "My Order 1", "MyOrder");
		formatAndParseOrder(order);
	}

	@Test
	public void shouldFormatAndParseVersionedOrder() throws Exception {
		Order order = ModelGenerator.createOrder("@1", "My Order 1", "MyOrder");
		Version.setInitialVersionFor(order, "test");
		Order parsed = formatAndParseOrder(order);
		assertEquals(order.getVersion(), parsed.getVersion());
	}

	@Test
	public void shouldFormatAndParseResource() throws Exception {
		Resource resource = ModelGenerator.createResource("@1", "My Resource 1", "MyResource");
		formatAndParseResource(resource);
	}

	@Test
	public void shouldFormatAndParseVersionedResource() throws Exception {
		Resource resource = ModelGenerator.createResource("@1", "My Resource 1", "MyResource");
		Version.setInitialVersionFor(resource, "test");
		Resource parsed = formatAndParseResource(resource);
		assertEquals(resource.getVersion(), parsed.getVersion());
	}

	@Test
	public void shouldFormatAndParseActivity() throws Exception {
		Activity activity = ModelGenerator.createActivity("@1", "My Activity 1", "Transport", TimeOrdering.SERIES);
		formatAndParseActivity(activity);
	}

	@Test
	public void shouldFormatAndParseVersionedActivity() throws Exception {
		Activity activity = ModelGenerator.createActivity("@1", "My Activity 1", "Transport", TimeOrdering.SERIES);
		Version.setInitialVersionFor(activity, "test");
		Activity parsed = formatAndParseActivity(activity);
		assertEquals(activity.getVersion(), parsed.getVersion());
	}

	protected abstract Order formatAndParseOrder(Order order) throws Exception;

	protected abstract Resource formatAndParseResource(Resource resource) throws Exception;

	protected abstract Activity formatAndParseActivity(Activity activity) throws Exception;

}
