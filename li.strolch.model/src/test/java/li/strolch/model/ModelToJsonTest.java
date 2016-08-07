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

import com.google.gson.JsonObject;

import li.strolch.model.activity.Activity;
import li.strolch.model.json.ActivityFromJsonVisitor;
import li.strolch.model.json.ActivityToJsonVisitor;
import li.strolch.model.json.OrderFromJsonVisitor;
import li.strolch.model.json.OrderToJsonVisitor;
import li.strolch.model.json.ResourceFromJsonVisitor;
import li.strolch.model.json.ResourceToJsonVisitor;
import li.strolch.model.visitor.ActivityDeepEqualsVisitor;
import li.strolch.model.visitor.OrderDeepEqualsVisitor;
import li.strolch.model.visitor.ResourceDeepEqualsVisitor;

public class ModelToJsonTest extends ModelMarshallingTest {

	@Override
	protected Order formatAndParseOrder(Order order) {
		OrderToJsonVisitor jsonVisitor = new OrderToJsonVisitor();
		jsonVisitor.visit(order);
		JsonObject jsonObject = jsonVisitor.getJsonObject();

		Order parsedOrder = new OrderFromJsonVisitor().visit(jsonObject);

		OrderDeepEqualsVisitor visitor = new OrderDeepEqualsVisitor(order);
		visitor.visit(parsedOrder);
		assertTrue("To JSON and back should equal same Order:\n" + visitor.getMismatchedLocators(), visitor.isEqual());

		return parsedOrder;
	}

	@Override
	protected Resource formatAndParseResource(Resource resource) {

		ResourceToJsonVisitor jsonVisitor = new ResourceToJsonVisitor();
		jsonVisitor.visit(resource);
		JsonObject jsonObject = jsonVisitor.getJsonObject();

		Resource parsedResource = new ResourceFromJsonVisitor().visit(jsonObject);

		ResourceDeepEqualsVisitor visitor = new ResourceDeepEqualsVisitor(resource);
		visitor.visit(parsedResource);
		assertTrue("To JSON and back should equal same Resource:\n" + visitor.getMismatchedLocators(),
				visitor.isEqual());

		return parsedResource;
	}

	@Override
	protected Activity formatAndParseActivity(Activity activity) {

		ActivityToJsonVisitor jsonVisitor = new ActivityToJsonVisitor();
		jsonVisitor.visit(activity);
		JsonObject jsonObject = jsonVisitor.getJsonObject();

		Activity parsedActivity = new ActivityFromJsonVisitor().visit(jsonObject);

		ActivityDeepEqualsVisitor visitor = new ActivityDeepEqualsVisitor(activity);
		visitor.visit(parsedActivity);
		assertTrue("To JSON and back should equal same Activity:\n" + visitor.getMismatchedLocators(),
				visitor.isEqual());

		return parsedActivity;
	}
}
