/*
 * Copyright 2016 Robert von Burg <eitch@eitchnet.ch>
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

import static org.junit.Assert.assertTrue;

import java.util.List;

import com.google.gson.JsonObject;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.model.json.ActivityFromJsonVisitor;
import li.strolch.model.json.OrderFromJsonVisitor;
import li.strolch.model.json.ResourceFromJsonVisitor;
import li.strolch.model.json.StrolchRootElementToJsonVisitor;
import li.strolch.model.visitor.StrolchElementDeepEqualsVisitor;
import org.junit.Test;

public class ModelToJsonTest extends ModelMarshallingTest {

	@Test
	public void shouldFormatAndParseExistingOrder() {
		Order order = ModelGenerator.createOrder("@1", "My Order 1", "MyOrder");
		StrolchRootElementToJsonVisitor jsonVisitor = new StrolchRootElementToJsonVisitor();
		JsonObject jsonObject = order.accept(jsonVisitor).getAsJsonObject();

		Order parsedOrder = order.getClone(true);
		new OrderFromJsonVisitor().overwriteExisting().fillElement(jsonObject, order);
		List<Locator> mismatches = parsedOrder.accept(new StrolchElementDeepEqualsVisitor(order));
		assertTrue("To JSON and back should equal same Order:\n" + mismatches, mismatches.isEmpty());
	}

	@Test
	public void shouldFormatAndParseExistingResource() {
		Resource resource = ModelGenerator.createResource("@1", "My Resource 1", "MyResource");
		StrolchRootElementToJsonVisitor jsonVisitor = new StrolchRootElementToJsonVisitor();
		JsonObject jsonObject = resource.accept(jsonVisitor).getAsJsonObject();

		Resource parsedResource = resource.getClone(true);
		new ResourceFromJsonVisitor().overwriteExisting().fillElement(jsonObject, resource);
		List<Locator> mismatches = parsedResource.accept(new StrolchElementDeepEqualsVisitor(resource));
		assertTrue("To JSON and back should equal same Resource:\n" + mismatches, mismatches.isEmpty());
	}

	@Test
	public void shouldFormatAndParseExistingActivity() {
		Activity activity = ModelGenerator.createActivity("@1", "My Activity 1", "Transport", TimeOrdering.SERIES);
		StrolchRootElementToJsonVisitor jsonVisitor = new StrolchRootElementToJsonVisitor();
		JsonObject jsonObject = activity.accept(jsonVisitor).getAsJsonObject();

		Activity parsedResource = activity.getClone(true);
		new ActivityFromJsonVisitor().overwriteExisting().fillElement(jsonObject, activity);
		List<Locator> mismatches = parsedResource.accept(new StrolchElementDeepEqualsVisitor(activity));
		assertTrue("To JSON and back should equal same Activity:\n" + mismatches, mismatches.isEmpty());
	}

	@Override
	protected Order formatAndParseOrder(Order order) {
		StrolchRootElementToJsonVisitor jsonVisitor = new StrolchRootElementToJsonVisitor();
		JsonObject jsonObject = order.accept(jsonVisitor).getAsJsonObject();

		Order parsedOrder = new OrderFromJsonVisitor().visit(jsonObject);
		List<Locator> mismatches = parsedOrder.accept(new StrolchElementDeepEqualsVisitor(order));
		assertTrue("To JSON and back should equal same Order:\n" + mismatches, mismatches.isEmpty());

		return parsedOrder;
	}

	@Override
	protected Resource formatAndParseResource(Resource resource) {
		StrolchRootElementToJsonVisitor jsonVisitor = new StrolchRootElementToJsonVisitor();
		JsonObject jsonObject = resource.accept(jsonVisitor).getAsJsonObject();

		Resource parsedResource = new ResourceFromJsonVisitor().visit(jsonObject);
		List<Locator> mismatches = parsedResource.accept(new StrolchElementDeepEqualsVisitor(resource));
		assertTrue("To JSON and back should equal same Resource:\n" + mismatches, mismatches.isEmpty());

		return parsedResource;
	}

	@Override
	protected Activity formatAndParseActivity(Activity activity) {
		StrolchRootElementToJsonVisitor jsonVisitor = new StrolchRootElementToJsonVisitor();
		JsonObject jsonObject = activity.accept(jsonVisitor).getAsJsonObject();

		Activity parsedActivity = new ActivityFromJsonVisitor().visit(jsonObject);
		List<Locator> mismatches = parsedActivity.accept(new StrolchElementDeepEqualsVisitor(activity));
		assertTrue("To JSON and back should equal same Activity:\n" + mismatches, mismatches.isEmpty());

		return parsedActivity;
	}
}
