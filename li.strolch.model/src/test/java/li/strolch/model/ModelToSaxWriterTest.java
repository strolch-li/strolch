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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.Collections;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;

import li.strolch.model.activity.Activity;
import li.strolch.model.visitor.ActivityDeepEqualsVisitor;
import li.strolch.model.visitor.OrderDeepEqualsVisitor;
import li.strolch.model.visitor.ResourceDeepEqualsVisitor;
import li.strolch.model.xml.ActivityToSaxWriterVisitor;
import li.strolch.model.xml.OrderToSaxWriterVisitor;
import li.strolch.model.xml.ResourceToSaxWriterVisitor;
import li.strolch.model.xml.SimpleStrolchElementListener;
import li.strolch.model.xml.XmlModelSaxStreamReader;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class ModelToSaxWriterTest extends ModelMarshallingTest {

	@Override
	protected Order formatAndParseOrder(Order order) throws Exception {

		XMLOutputFactory factory = XMLOutputFactory.newInstance();
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		XMLStreamWriter writer = factory.createXMLStreamWriter(out, "UTF-8");
		writer.writeStartDocument();
		writer.writeStartElement(Tags.STROLCH_MODEL);
		OrderToSaxWriterVisitor toSax = new OrderToSaxWriterVisitor(writer);
		toSax.visit(order);
		writer.writeEndDocument();

		SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
		XmlModelSaxStreamReader saxStreamReader = new XmlModelSaxStreamReader(listener,
				new ByteArrayInputStream(out.toByteArray()));
		saxStreamReader.parseStream();

		assertEquals(1, listener.getOrders().size());
		assertEquals(Collections.emptyList(), listener.getResources());
		assertEquals(Collections.emptyList(), listener.getActivities());
		Order parsedOrder = listener.getOrders().get(0);

		OrderDeepEqualsVisitor visitor = new OrderDeepEqualsVisitor(order);
		visitor.visit(parsedOrder);
		assertTrue("To DOM and back should equal same Order:\n" + visitor.getMismatchedLocators(), visitor.isEqual());

		return parsedOrder;
	}

	@Override
	protected Resource formatAndParseResource(Resource resource) throws Exception {

		XMLOutputFactory factory = XMLOutputFactory.newInstance();
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		XMLStreamWriter writer = factory.createXMLStreamWriter(out, "UTF-8");
		writer.writeStartDocument();
		writer.writeStartElement(Tags.STROLCH_MODEL);
		ResourceToSaxWriterVisitor toSax = new ResourceToSaxWriterVisitor(writer);
		toSax.visit(resource);
		writer.writeEndDocument();

		SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
		XmlModelSaxStreamReader saxStreamReader = new XmlModelSaxStreamReader(listener,
				new ByteArrayInputStream(out.toByteArray()));
		saxStreamReader.parseStream();

		assertEquals(1, listener.getResources().size());
		assertEquals(Collections.emptyList(), listener.getActivities());
		assertEquals(Collections.emptyList(), listener.getOrders());
		Resource parsedResource = listener.getResources().get(0);

		ResourceDeepEqualsVisitor visitor = new ResourceDeepEqualsVisitor(resource);
		visitor.visit(parsedResource);
		assertTrue("To DOM and back should equal same Resource:\n" + visitor.getMismatchedLocators(),
				visitor.isEqual());

		return parsedResource;
	}
	

	@Override
	protected Activity formatAndParseActivity(Activity activity) throws Exception {

		XMLOutputFactory factory = XMLOutputFactory.newInstance();
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		XMLStreamWriter writer = factory.createXMLStreamWriter(out, "UTF-8");
		writer.writeStartDocument();
		writer.writeStartElement(Tags.STROLCH_MODEL);
		ActivityToSaxWriterVisitor toSax = new ActivityToSaxWriterVisitor(writer);
		toSax.visit(activity);
		writer.writeEndDocument();
		
		System.out.println(out.toString());

		SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
		XmlModelSaxStreamReader saxStreamReader = new XmlModelSaxStreamReader(listener,
				new ByteArrayInputStream(out.toByteArray()));
		saxStreamReader.parseStream();

		assertEquals(1, listener.getActivities().size());
		assertEquals(Collections.emptyList(), listener.getResources());
		assertEquals(Collections.emptyList(), listener.getOrders());
		Activity parsedActivity = listener.getActivities().get(0);

		ActivityDeepEqualsVisitor visitor = new ActivityDeepEqualsVisitor(activity);
		visitor.visit(parsedActivity);
		assertTrue("To DOM and back should equal same Activity:\n" + visitor.getMismatchedLocators(),
				visitor.isEqual());

		return parsedActivity;
	}
}
