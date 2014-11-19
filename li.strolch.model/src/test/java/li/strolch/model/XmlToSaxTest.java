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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import li.strolch.model.visitor.OrderDeepEqualsVisitor;
import li.strolch.model.visitor.ResourceDeepEqualsVisitor;
import li.strolch.model.xml.OrderToSaxVisitor;
import li.strolch.model.xml.ResourceToSaxVisitor;
import li.strolch.model.xml.SimpleStrolchElementListener;
import li.strolch.model.xml.XmlModelSaxReader;

import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class XmlToSaxTest extends ModelTest {

	@Test
	public void shouldFormatAndParseOrder() {

		Order order = ModelGenerator.createOrder("@1", "My Order 1", "MyOrder");

		SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
		XmlModelSaxReader saxReader = new XmlModelSaxReader(listener);

		OrderToSaxVisitor domVisitor = new OrderToSaxVisitor(saxReader);
		domVisitor.visit(order);

		assertEquals(1, listener.getOrders().size());
		assertNull(listener.getResources());
		Order parsedOrder = listener.getOrders().get(0);

		OrderDeepEqualsVisitor visitor = new OrderDeepEqualsVisitor(order);
		visitor.visit(parsedOrder);
		assertTrue("To DOM and back should equal same Order:\n" + visitor.getMismatchedLocators(), visitor.isEqual());
	}

	@Test
	public void shouldFormatAndParseResource() {

		Resource resource = ModelGenerator.createResource("@1", "My Resource 1", "MyResource");

		SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
		XmlModelSaxReader saxReader = new XmlModelSaxReader(listener);

		ResourceToSaxVisitor domVisitor = new ResourceToSaxVisitor(saxReader);
		domVisitor.visit(resource);

		assertEquals(1, listener.getResources().size());
		assertNull(listener.getOrders());
		Resource parsedResource = listener.getResources().get(0);

		ResourceDeepEqualsVisitor visitor = new ResourceDeepEqualsVisitor(resource);
		visitor.visit(parsedResource);
		assertTrue("To DOM and back should equal same Resource:\n" + visitor.getMismatchedLocators(), visitor.isEqual());
	}
}
