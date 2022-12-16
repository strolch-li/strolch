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
package li.strolch.model;

import static org.junit.Assert.assertTrue;

import java.util.List;

import org.w3c.dom.Document;

import li.strolch.model.activity.Activity;
import li.strolch.model.visitor.StrolchElementDeepEqualsVisitor;
import li.strolch.model.xml.ActivityFromDomVisitor;
import li.strolch.model.xml.OrderFromDomVisitor;
import li.strolch.model.xml.ResourceFromDomVisitor;
import li.strolch.model.xml.StrolchElementToDomVisitor;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class ModelToDomTest extends ModelMarshallingTest {

	protected Resource formatAndParseResource(Resource resource) {

		Document document = resource.accept(new StrolchElementToDomVisitor());

		Resource parsedResource = new ResourceFromDomVisitor().visit(document);

		StrolchElementDeepEqualsVisitor visitor = new StrolchElementDeepEqualsVisitor(resource);
		List<Locator> mismatches = parsedResource.accept(visitor);
		assertTrue("To DOM and back should equal same Resource:\n" + mismatches, mismatches.isEmpty());

		return parsedResource;
	}

	protected Order formatAndParseOrder(Order order) {

		Document document = order.accept(new StrolchElementToDomVisitor());

		Order parsedOrder = new OrderFromDomVisitor().visit(document);

		StrolchElementDeepEqualsVisitor visitor = new StrolchElementDeepEqualsVisitor(order);
		List<Locator> mismatches = parsedOrder.accept(visitor);
		assertTrue("To DOM and back should equal same Order:\n" + mismatches, mismatches.isEmpty());

		return parsedOrder;
	}

	protected Activity formatAndParseActivity(Activity activity) {

		Document document = activity.accept(new StrolchElementToDomVisitor());

		Activity parsedActivity = new ActivityFromDomVisitor().visit(document);

		StrolchElementDeepEqualsVisitor visitor = new StrolchElementDeepEqualsVisitor(activity);
		List<Locator> mismatches = parsedActivity.accept(visitor);
		assertTrue("To DOM and back should equal same Activity:\n" + mismatches, mismatches.isEmpty());

		return parsedActivity;
	}
}
