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
package ch.eitchnet.xmlpers.test.impl;

import javax.xml.stream.XMLStreamException;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import ch.eitchnet.xmlpers.api.SaxParser;
import ch.eitchnet.xmlpers.api.XmlPersistenceStreamWriter;
import ch.eitchnet.xmlpers.test.model.Parameter;
import ch.eitchnet.xmlpers.test.model.Resource;

class ResourceSaxParser extends DefaultHandler implements SaxParser<Resource> {

	private Resource resource;

	@Override
	public Resource getObject() {
		return this.resource;
	}

	@Override
	public void setObject(Resource object) {
		this.resource = object;
	}

	@Override
	public DefaultHandler getDefaultHandler() {
		return this;
	}

	@SuppressWarnings("nls")
	@Override
	public void write(XmlPersistenceStreamWriter writer) throws XMLStreamException {

		writer.writeElement("Resource");
		writer.writeAttribute("id", this.resource.getId());
		writer.writeAttribute("name", this.resource.getName());
		writer.writeAttribute("type", this.resource.getType());
		for (String paramId : this.resource.getParameterKeySet()) {
			Parameter param = this.resource.getParameterBy(paramId);
			writer.writeEmptyElement("Parameter");
			writer.writeAttribute("id", param.getId());
			writer.writeAttribute("name", param.getName());
			writer.writeAttribute("type", param.getType());
			writer.writeAttribute("value", param.getValue());
		}
	}

	@SuppressWarnings("nls")
	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {

		switch (qName) {
		case "Resource":
			String id = attributes.getValue("id");
			String name = attributes.getValue("name");
			String type = attributes.getValue("type");
			Resource resource = new Resource(id, name, type);
			this.resource = resource;
			break;
		case "Parameter":
			id = attributes.getValue("id");
			name = attributes.getValue("name");
			type = attributes.getValue("type");
			String value = attributes.getValue("value");
			Parameter param = new Parameter(id, name, type, value);
			this.resource.addParameter(param);
			break;
		default:
			throw new IllegalArgumentException("The element '" + qName + "' is unhandled!");
		}
	}
}