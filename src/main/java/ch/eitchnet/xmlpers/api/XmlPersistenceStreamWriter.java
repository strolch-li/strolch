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
package ch.eitchnet.xmlpers.api;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class XmlPersistenceStreamWriter {

	private XMLStreamWriter writer;

	public XmlPersistenceStreamWriter(XMLStreamWriter writer) {
		this.writer = writer;
	}

	/**
	 * @param localName
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeEmptyElement(java.lang.String)
	 */
	public void writeEmptyElement(String localName) throws XMLStreamException {
		this.writer.writeEmptyElement(localName);
	}

	/**
	 * @param localName
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeStartElement(java.lang.String)
	 */
	public void writeElement(String localName) throws XMLStreamException {
		this.writer.writeStartElement(localName);
	}

	/**
	 * <b>Note:</b> Don't call this method to close an element written by {@link #writeEmptyElement(String)}
	 * 
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeEndElement()
	 */
	public void endElement() throws XMLStreamException {
		this.writer.writeEndElement();
	}

	/**
	 * @param localName
	 * @param value
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeAttribute(java.lang.String, java.lang.String)
	 */
	public void writeAttribute(String localName, String value) throws XMLStreamException {
		this.writer.writeAttribute(localName, value);
	}

	/**
	 * @param data
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeCData(java.lang.String)
	 */
	public void writeCData(String data) throws XMLStreamException {
		this.writer.writeCData(data);
	}

	/**
	 * @param text
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeCharacters(java.lang.String)
	 */
	public void writeCharacters(String text) throws XMLStreamException {
		this.writer.writeCharacters(text);
	}
}
