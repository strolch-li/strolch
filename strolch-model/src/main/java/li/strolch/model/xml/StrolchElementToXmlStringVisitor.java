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

package li.strolch.model.xml;

import javanet.staxutils.IndentingXMLStreamWriter;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.activity.Activity;
import li.strolch.model.visitor.StrolchRootElementVisitor;
import li.strolch.utils.dbc.DBC;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;
import java.io.StringWriter;

import static li.strolch.model.StrolchModelConstants.DEFAULT_ENCODING;
import static li.strolch.model.StrolchModelConstants.DEFAULT_XML_VERSION;

public class StrolchElementToXmlStringVisitor implements StrolchRootElementVisitor<String> {

	private boolean withDocument;

	public StrolchElementToXmlStringVisitor() {
		this.withDocument = true;
	}

	public StrolchElementToXmlStringVisitor withoutDocument() {
		this.withDocument = false;
		return this;
	}

	private String visit(StrolchRootElement element) {

		try {
			StringWriter stringWriter = new StringWriter();
			XMLOutputFactory factory = XMLOutputFactory.newInstance();
			XMLStreamWriter writer = factory.createXMLStreamWriter(stringWriter);
			writer = new IndentingXMLStreamWriter(writer);

			// start document
			if (this.withDocument)
				writer.writeStartDocument(DEFAULT_ENCODING, DEFAULT_XML_VERSION);

			element.accept(new StrolchElementToSaxWriterVisitor(writer));

			if (this.withDocument)
				writer.writeEndDocument();

			return stringWriter.toString();

		} catch (Exception e) {
			throw new RuntimeException(
					"Failed to format Element " + element.getLocator() + " to xml string due to " + e.getMessage(), e);
		}
	}

	@Override
	public String visitOrder(Order element) {
		DBC.PRE.assertNotNull("Order my not be null!", element);
		return visit(element);
	}

	@Override
	public String visitResource(Resource element) {
		DBC.PRE.assertNotNull("Resource my not be null!", element);
		return visit(element);
	}

	@Override
	public String visitActivity(Activity element) {
		DBC.PRE.assertNotNull("Activity my not be null!", element);
		return visit(element);
	}
}
