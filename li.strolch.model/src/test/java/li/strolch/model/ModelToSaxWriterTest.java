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

import javax.xml.stream.XMLStreamWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.Collections;
import java.util.List;

import li.strolch.model.activity.Activity;
import li.strolch.model.visitor.StrolchElementDeepEqualsVisitor;
import li.strolch.model.xml.SimpleStrolchElementListener;
import li.strolch.model.xml.StrolchElementToSaxWriterVisitor;
import li.strolch.model.xml.StrolchXmlHelper;
import li.strolch.model.xml.XmlModelSaxStreamReader;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class ModelToSaxWriterTest extends ModelMarshallingTest {

	@Override
	protected Order formatAndParseOrder(Order order) throws Exception {

		ByteArrayOutputStream out = new ByteArrayOutputStream();
		XMLStreamWriter writer = StrolchXmlHelper.prepareXmlStreamWriter(out);
		order.accept(new StrolchElementToSaxWriterVisitor(writer));
		writer.writeEndDocument();

		SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
		XmlModelSaxStreamReader saxStreamReader = new XmlModelSaxStreamReader(listener,
				new ByteArrayInputStream(out.toByteArray()));
		saxStreamReader.parseStream();

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
	protected Resource formatAndParseResource(Resource resource) throws Exception {

		ByteArrayOutputStream out = new ByteArrayOutputStream();
		XMLStreamWriter writer = StrolchXmlHelper.prepareXmlStreamWriter(out);
		resource.accept(new StrolchElementToSaxWriterVisitor(writer));
		writer.writeEndDocument();

		SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
		XmlModelSaxStreamReader saxStreamReader = new XmlModelSaxStreamReader(listener,
				new ByteArrayInputStream(out.toByteArray()));
		saxStreamReader.parseStream();

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
	protected Activity formatAndParseActivity(Activity activity) throws Exception {

		ByteArrayOutputStream out = new ByteArrayOutputStream();
		XMLStreamWriter writer = StrolchXmlHelper.prepareXmlStreamWriter(out);
		activity.accept(new StrolchElementToSaxWriterVisitor(writer));
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

		StrolchElementDeepEqualsVisitor visitor = new StrolchElementDeepEqualsVisitor(activity);
		List<Locator> mismatches = parsedActivity.accept(visitor);
		assertTrue("To DOM and back should equal same Activity:\n" + mismatches, mismatches.isEmpty());

		return parsedActivity;
	}
}
