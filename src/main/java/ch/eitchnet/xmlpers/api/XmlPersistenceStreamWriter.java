/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
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
