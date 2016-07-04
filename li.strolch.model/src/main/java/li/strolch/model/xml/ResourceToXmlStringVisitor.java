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
package li.strolch.model.xml;

import java.io.StringWriter;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;

import javanet.staxutils.IndentingXMLStreamWriter;
import li.strolch.model.Resource;
import li.strolch.model.ResourceVisitor;
import li.strolch.model.StrolchModelConstants;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ResourceToXmlStringVisitor implements ResourceVisitor<String> {

	@Override
	public String visit(Resource element) {
		DBC.PRE.assertNotNull("Resource my not be null!", element);
		try {
			StringWriter stringWriter = new StringWriter();
			XMLOutputFactory factory = XMLOutputFactory.newInstance();
			XMLStreamWriter writer = factory.createXMLStreamWriter(stringWriter);
			writer = new IndentingXMLStreamWriter(writer);

			// start document
			writer.writeStartDocument(StrolchModelConstants.DEFAULT_ENCODING,
					StrolchModelConstants.DEFAULT_XML_VERSION);
			new ResourceToSaxWriterVisitor(writer).visit(element);
			writer.writeEndDocument();

			return stringWriter.toString();
		} catch (Exception e) {
			throw new RuntimeException(
					"Failed to format Element " + element.getLocator() + " to xml string due to " + e.getMessage(), e);
		}
	}
}
