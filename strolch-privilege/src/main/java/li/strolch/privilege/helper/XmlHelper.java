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

package li.strolch.privilege.helper;

import javanet.staxutils.IndentingXMLStreamWriter;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static li.strolch.privilege.helper.XmlConstants.ATTR_NAME;
import static li.strolch.privilege.helper.XmlConstants.ATTR_VALUE;

public class XmlHelper {

	public static void writeStringMapElement(XMLStreamWriter xmlWriter, Map<String, String> parameterMap,
			String elementName, String valueElementName) throws XMLStreamException {
		writeStringMapElement(xmlWriter, parameterMap, elementName, valueElementName, ATTR_VALUE);
	}

	public static void writeStringMapElement(XMLStreamWriter xmlWriter, Map<String, String> parameterMap,
			String elementName, String valueElementName, String valueAttrName) throws XMLStreamException {
		if (parameterMap == null || parameterMap.isEmpty())
			return;

		xmlWriter.writeStartElement(elementName);

		List<String> propertyKeys = new ArrayList<>(parameterMap.keySet());
		propertyKeys.sort(null);
		for (String propertyKey : propertyKeys) {
			xmlWriter.writeEmptyElement(valueElementName);
			xmlWriter.writeAttribute(ATTR_NAME, propertyKey);
			xmlWriter.writeAttribute(valueAttrName, parameterMap.get(propertyKey));
		}

		xmlWriter.writeEndElement();
	}

	public static void writeStringList(IndentingXMLStreamWriter xmlWriter, String elementName, Set<String> values)
			throws XMLStreamException {
		List<String> denyList = new ArrayList<>(values);
		denyList.sort(null);
		for (String value : denyList) {
			writeStringElement(xmlWriter, elementName, value);
		}
	}

	public static void writeStringElement(IndentingXMLStreamWriter xmlWriter, String elementName, String value)
			throws XMLStreamException {
		xmlWriter.writeStartElement(elementName);
		xmlWriter.writeCharacters(value);
		xmlWriter.writeEndElement();
	}

	public static IndentingXMLStreamWriter openXmlStreamWriterDocument(Writer ioWriter) throws XMLStreamException {
		XMLOutputFactory factory = XMLOutputFactory.newInstance();
		IndentingXMLStreamWriter xmlWriter = new IndentingXMLStreamWriter(factory.createXMLStreamWriter(ioWriter));
		xmlWriter.setIndent("    ");

		// create document root
		xmlWriter.writeStartDocument(StandardCharsets.UTF_8.name(), "1.0");
		return xmlWriter;
	}
}
