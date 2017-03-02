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

import org.w3c.dom.Document;

import li.strolch.model.activity.Activity;
import li.strolch.model.visitor.ActivityDeepEqualsVisitor;
import li.strolch.model.visitor.OrderDeepEqualsVisitor;
import li.strolch.model.visitor.ResourceDeepEqualsVisitor;
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

		ResourceDeepEqualsVisitor visitor = new ResourceDeepEqualsVisitor(resource);
		visitor.visit(parsedResource);
		assertTrue("To DOM and back should equal same Resource:\n" + visitor.getMismatchedLocators(),
				visitor.isEqual());

		return parsedResource;
	}

	protected Order formatAndParseOrder(Order order) {

		Document document = order.accept(new StrolchElementToDomVisitor());

		Order parsedOrder = new OrderFromDomVisitor().visit(document);

		OrderDeepEqualsVisitor visitor = new OrderDeepEqualsVisitor(order);
		visitor.visit(parsedOrder);
		assertTrue("To DOM and back should equal same Order:\n" + visitor.getMismatchedLocators(), visitor.isEqual());

		return parsedOrder;
	}

	protected Activity formatAndParseActivity(Activity activity) {

		Document document = activity.accept(new StrolchElementToDomVisitor());

		Activity parsedActivity = new ActivityFromDomVisitor().visit(document);

		ActivityDeepEqualsVisitor visitor = new ActivityDeepEqualsVisitor(activity);
		visitor.visit(parsedActivity);
		assertTrue("To DOM and back should equal same Activity:\n" + visitor.getMismatchedLocators(),
				visitor.isEqual());

		return parsedActivity;
	}
}
