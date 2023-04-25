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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Collections;
import java.util.List;

import li.strolch.model.activity.Activity;
import li.strolch.model.visitor.StrolchElementDeepEqualsVisitor;
import li.strolch.model.xml.SimpleStrolchElementListener;
import li.strolch.model.xml.StrolchElementToSaxVisitor;
import li.strolch.model.xml.XmlModelSaxReader;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ModelToSaxTest extends ModelMarshallingTest {

	@Override
	protected Order formatAndParseOrder(Order order) {

		SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
		XmlModelSaxReader saxReader = new XmlModelSaxReader(listener);

		order.accept(new StrolchElementToSaxVisitor(saxReader));

		assertEquals(1, listener.getOrders().size());
		assertEquals(Collections.emptyList(), listener.getResources());
		assertEquals(Collections.emptyList(), listener.getActivities());
		Order parsedOrder = listener.getOrders().get(0);

		StrolchElementDeepEqualsVisitor visitor = new StrolchElementDeepEqualsVisitor(order);
		List<Locator> mismatches = parsedOrder.accept(visitor);
		assertTrue("To DOM and back should equal same Order:\n" + mismatches, mismatches.isEmpty());

		return parsedOrder;
	}

	@Override
	protected Resource formatAndParseResource(Resource resource) {

		SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
		XmlModelSaxReader saxReader = new XmlModelSaxReader(listener);

		resource.accept(new StrolchElementToSaxVisitor(saxReader));

		assertEquals(1, listener.getResources().size());
		assertEquals(Collections.emptyList(), listener.getActivities());
		assertEquals(Collections.emptyList(), listener.getOrders());
		Resource parsedResource = listener.getResources().get(0);

		StrolchElementDeepEqualsVisitor visitor = new StrolchElementDeepEqualsVisitor(resource);
		List<Locator> mismatches = parsedResource.accept(visitor);
		assertTrue("To DOM and back should equal same Resource:\n" + mismatches, mismatches.isEmpty());

		return parsedResource;
	}

	@Override
	protected Activity formatAndParseActivity(Activity activity) {

		SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
		XmlModelSaxReader saxReader = new XmlModelSaxReader(listener);

		activity.accept(new StrolchElementToSaxVisitor(saxReader));

		assertEquals(1, listener.getActivities().size());
		assertEquals(Collections.emptyList(), listener.getResources());
		assertEquals(Collections.emptyList(), listener.getOrders());
		Activity parsedActivity = listener.getActivities().get(0);

		StrolchElementDeepEqualsVisitor visitor = new StrolchElementDeepEqualsVisitor(activity);
		List<Locator> mismatches = parsedActivity.accept(visitor);
		assertTrue("To DOM and back should equal same Activity:\n" + mismatches, mismatches.isEmpty());

		return parsedActivity;
	}
}
