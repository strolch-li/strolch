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
package li.strolch.xmlpers.test.impl;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import li.strolch.xmlpers.api.SaxParser;
import li.strolch.xmlpers.test.model.MyModel;
import li.strolch.xmlpers.test.model.MyParameter;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

class MyModelSaxParser extends DefaultHandler implements SaxParser<MyModel> {

	private MyModel resource;

	@Override
	public MyModel getObject() {
		return this.resource;
	}

	@Override
	public void setObject(MyModel object) {
		this.resource = object;
	}

	@Override
	public DefaultHandler getDefaultHandler() {
		return this;
	}

	@Override
	public void write(XMLStreamWriter writer) throws XMLStreamException {

		writer.writeStartElement("Resource");
		writer.writeAttribute("id", this.resource.getId());
		writer.writeAttribute("name", this.resource.getName());
		writer.writeAttribute("type", this.resource.getType());
		for (String paramId : this.resource.getParameterKeySet()) {
			MyParameter param = this.resource.getParameterBy(paramId);
			writer.writeEmptyElement("Parameter");
			writer.writeAttribute("id", param.getId());
			writer.writeAttribute("name", param.getName());
			writer.writeAttribute("type", param.getType());
			writer.writeAttribute("value", param.getValue());
		}
		writer.writeEndElement();
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) {

		switch (qName) {
		case "Resource" -> {
			String id = attributes.getValue("id");
			String name = attributes.getValue("name");
			String type = attributes.getValue("type");
			this.resource = new MyModel(id, name, type);
		}
		case "Parameter" -> {
			String id = attributes.getValue("id");
			String name = attributes.getValue("name");
			String type = attributes.getValue("type");
			String value = attributes.getValue("value");
			this.resource.addParameter(new MyParameter(id, name, type, value));
		}
		default -> throw new IllegalArgumentException("The element '" + qName + "' is unhandled!");
		}
	}
}