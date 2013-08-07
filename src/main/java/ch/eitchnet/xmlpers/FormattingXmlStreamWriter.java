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
package ch.eitchnet.xmlpers;

import javax.xml.namespace.NamespaceContext;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class FormattingXmlStreamWriter implements XMLStreamWriter {

	private final XMLStreamWriter writer;

	/**
	 * 
	 * @param writer
	 */
	public FormattingXmlStreamWriter(XMLStreamWriter writer) {
		this.writer = writer;
	}

	//
	// Start of elements
	//

	/**
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeStartDocument()
	 */
	@Override
	public void writeStartDocument() throws XMLStreamException {
		preStart();
		this.writer.writeStartDocument();
		postStart();
	}

	private void preStart() {

	}

	private void postStart() throws XMLStreamException {
		//this.writer.writeCharacters(new char[] { '\n' }, 0, 1);
	}

	private void preEnd() {
	}

	private void postEnd() {
	}

	/**
	 * @param version
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeStartDocument(java.lang.String)
	 */
	@Override
	public void writeStartDocument(String version) throws XMLStreamException {
		preStart();
		this.writer.writeStartDocument(version);
		postStart();
	}

	/**
	 * @param encoding
	 * @param version
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeStartDocument(java.lang.String, java.lang.String)
	 */
	@Override
	public void writeStartDocument(String encoding, String version) throws XMLStreamException {
		preStart();
		this.writer.writeStartDocument(encoding, version);
		postStart();
	}

	/**
	 * @param localName
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeStartElement(java.lang.String)
	 */
	@Override
	public void writeStartElement(String localName) throws XMLStreamException {
		preStart();
		this.writer.writeStartElement(localName);
		postStart();
	}

	/**
	 * @param namespaceURI
	 * @param localName
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeStartElement(java.lang.String, java.lang.String)
	 */
	@Override
	public void writeStartElement(String namespaceURI, String localName) throws XMLStreamException {
		preStart();
		this.writer.writeStartElement(namespaceURI, localName);
		postStart();
	}

	/**
	 * @param prefix
	 * @param localName
	 * @param namespaceURI
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeStartElement(java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override
	public void writeStartElement(String prefix, String localName, String namespaceURI) throws XMLStreamException {
		preStart();
		this.writer.writeStartElement(prefix, localName, namespaceURI);
		postStart();
	}

	//
	// End of elements
	//

	/**
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeEndElement()
	 */
	@Override
	public void writeEndElement() throws XMLStreamException {
		preStart();
		this.writer.writeEndElement();
		postStart();
	}

	/**
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeEndDocument()
	 */
	@Override
	public void writeEndDocument() throws XMLStreamException {
		preStart();
		this.writer.writeEndDocument();
		postStart();
	}

	//
	// Empty elements
	//

	/**
	 * @param namespaceURI
	 * @param localName
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeEmptyElement(java.lang.String, java.lang.String)
	 */
	@Override
	public void writeEmptyElement(String namespaceURI, String localName) throws XMLStreamException {
		preEnd();
		this.writer.writeEmptyElement(namespaceURI, localName);
		postEnd();
	}

	/**
	 * @param prefix
	 * @param localName
	 * @param namespaceURI
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeEmptyElement(java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override
	public void writeEmptyElement(String prefix, String localName, String namespaceURI) throws XMLStreamException {
		preEnd();
		this.writer.writeEmptyElement(prefix, localName, namespaceURI);
		postEnd();
	}

	/**
	 * @param localName
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeEmptyElement(java.lang.String)
	 */
	@Override
	public void writeEmptyElement(String localName) throws XMLStreamException {
		this.writer.writeEmptyElement(localName);
	}

	//
	// attributes
	//

	/**
	 * @param localName
	 * @param value
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeAttribute(java.lang.String, java.lang.String)
	 */
	@Override
	public void writeAttribute(String localName, String value) throws XMLStreamException {
		this.writer.writeAttribute(localName, value);
	}

	/**
	 * @param prefix
	 * @param namespaceURI
	 * @param localName
	 * @param value
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeAttribute(java.lang.String, java.lang.String, java.lang.String,
	 *      java.lang.String)
	 */
	@Override
	public void writeAttribute(String prefix, String namespaceURI, String localName, String value)
			throws XMLStreamException {
		this.writer.writeAttribute(prefix, namespaceURI, localName, value);
	}

	/**
	 * @param namespaceURI
	 * @param localName
	 * @param value
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeAttribute(java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override
	public void writeAttribute(String namespaceURI, String localName, String value) throws XMLStreamException {
		this.writer.writeAttribute(namespaceURI, localName, value);
	}

	//
	// other
	//

	/**
	 * @param data
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeComment(java.lang.String)
	 */
	@Override
	public void writeComment(String data) throws XMLStreamException {
		this.writer.writeComment(data);
	}

	/**
	 * @param target
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeProcessingInstruction(java.lang.String)
	 */
	@Override
	public void writeProcessingInstruction(String target) throws XMLStreamException {
		this.writer.writeProcessingInstruction(target);
	}

	/**
	 * @param target
	 * @param data
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeProcessingInstruction(java.lang.String, java.lang.String)
	 */
	@Override
	public void writeProcessingInstruction(String target, String data) throws XMLStreamException {
		this.writer.writeProcessingInstruction(target, data);
	}

	/**
	 * @param data
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeCData(java.lang.String)
	 */
	@Override
	public void writeCData(String data) throws XMLStreamException {
		this.writer.writeCData(data);
	}

	/**
	 * @param dtd
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeDTD(java.lang.String)
	 */
	@Override
	public void writeDTD(String dtd) throws XMLStreamException {
		this.writer.writeDTD(dtd);
	}

	/**
	 * @param prefix
	 * @param namespaceURI
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeNamespace(java.lang.String, java.lang.String)
	 */
	@Override
	public void writeNamespace(String prefix, String namespaceURI) throws XMLStreamException {
		this.writer.writeNamespace(prefix, namespaceURI);
	}

	/**
	 * @param namespaceURI
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeDefaultNamespace(java.lang.String)
	 */
	@Override
	public void writeDefaultNamespace(String namespaceURI) throws XMLStreamException {
		this.writer.writeDefaultNamespace(namespaceURI);
	}

	/**
	 * @param name
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeEntityRef(java.lang.String)
	 */
	@Override
	public void writeEntityRef(String name) throws XMLStreamException {
		this.writer.writeEntityRef(name);
	}

	/**
	 * @param text
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeCharacters(java.lang.String)
	 */
	@Override
	public void writeCharacters(String text) throws XMLStreamException {
		this.writer.writeCharacters(text);
	}

	/**
	 * @param text
	 * @param start
	 * @param len
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#writeCharacters(char[], int, int)
	 */
	@Override
	public void writeCharacters(char[] text, int start, int len) throws XMLStreamException {
		this.writer.writeCharacters(text, start, len);
	}

	/**
	 * @param uri
	 * @return
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#getPrefix(java.lang.String)
	 */
	@Override
	public String getPrefix(String uri) throws XMLStreamException {
		return this.writer.getPrefix(uri);
	}

	/**
	 * @param prefix
	 * @param uri
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#setPrefix(java.lang.String, java.lang.String)
	 */
	@Override
	public void setPrefix(String prefix, String uri) throws XMLStreamException {
		this.writer.setPrefix(prefix, uri);
	}

	/**
	 * @param uri
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#setDefaultNamespace(java.lang.String)
	 */
	@Override
	public void setDefaultNamespace(String uri) throws XMLStreamException {
		this.writer.setDefaultNamespace(uri);
	}

	/**
	 * @param context
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#setNamespaceContext(javax.xml.namespace.NamespaceContext)
	 */
	@Override
	public void setNamespaceContext(NamespaceContext context) throws XMLStreamException {
		this.writer.setNamespaceContext(context);
	}

	/**
	 * @return
	 * @see javax.xml.stream.XMLStreamWriter#getNamespaceContext()
	 */
	@Override
	public NamespaceContext getNamespaceContext() {
		return this.writer.getNamespaceContext();
	}

	/**
	 * @param name
	 * @return
	 * @throws IllegalArgumentException
	 * @see javax.xml.stream.XMLStreamWriter#getProperty(java.lang.String)
	 */
	@Override
	public Object getProperty(String name) throws IllegalArgumentException {
		return this.writer.getProperty(name);
	}

	/**
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#close()
	 */
	@Override
	public void close() throws XMLStreamException {
		this.writer.close();
	}

	/**
	 * @throws XMLStreamException
	 * @see javax.xml.stream.XMLStreamWriter#flush()
	 */
	@Override
	public void flush() throws XMLStreamException {
		this.writer.flush();
	}
}
