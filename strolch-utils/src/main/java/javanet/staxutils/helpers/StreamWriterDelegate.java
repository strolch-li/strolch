/*
 * Copyright (c) 2006, John Kristian
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *      *   Redistributions of source code must retain the above copyright
 *          notice, this list of conditions and the following disclaimer.
 *
 *      *   Redistributions in binary form must reproduce the above copyright
 *          notice, this list of conditions and the following disclaimer in the
 *          documentation and/or other materials provided with the distribution.
 *
 *      *   Neither the name of StAX-Utils nor the names of its contributors
 *          may be used to endorse or promote products derived from this
 *          software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */
package javanet.staxutils.helpers;

import javax.xml.namespace.NamespaceContext;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

/**
 * Abstract class for writing filtered XML streams. This class provides methods that merely delegate to the contained
 * stream. Subclasses should override some of these methods, and may also provide additional methods and fields.
 *
 * @author <a href="mailto:jk2006@engineer.com">John Kristian</a>
 */
public abstract class StreamWriterDelegate implements XMLStreamWriter {

	protected StreamWriterDelegate(XMLStreamWriter out) {
		this.out = out;
	}

	protected final XMLStreamWriter out;

	@Override
	public Object getProperty(String name) throws IllegalArgumentException {
		return this.out.getProperty(name);
	}

	@Override
	public NamespaceContext getNamespaceContext() {
		return this.out.getNamespaceContext();
	}

	@Override
	public void setNamespaceContext(NamespaceContext context) throws XMLStreamException {
		this.out.setNamespaceContext(context);
	}

	@Override
	public void setDefaultNamespace(String uri) throws XMLStreamException {
		this.out.setDefaultNamespace(uri);
	}

	@Override
	public void writeStartDocument() throws XMLStreamException {
		this.out.writeStartDocument();
	}

	@Override
	public void writeStartDocument(String version) throws XMLStreamException {
		this.out.writeStartDocument(version);
	}

	@Override
	public void writeStartDocument(String encoding, String version) throws XMLStreamException {
		this.out.writeStartDocument(encoding, version);
	}

	@Override
	public void writeDTD(String dtd) throws XMLStreamException {
		this.out.writeDTD(dtd);
	}

	@Override
	public void writeProcessingInstruction(String target) throws XMLStreamException {
		this.out.writeProcessingInstruction(target);
	}

	@Override
	public void writeProcessingInstruction(String target, String data) throws XMLStreamException {
		this.out.writeProcessingInstruction(target, data);
	}

	@Override
	public void writeComment(String data) throws XMLStreamException {
		this.out.writeComment(data);
	}

	@Override
	public void writeEmptyElement(String localName) throws XMLStreamException {
		this.out.writeEmptyElement(localName);
	}

	@Override
	public void writeEmptyElement(String namespaceURI, String localName) throws XMLStreamException {
		this.out.writeEmptyElement(namespaceURI, localName);
	}

	@Override
	public void writeEmptyElement(String prefix, String localName, String namespaceURI) throws XMLStreamException {
		this.out.writeEmptyElement(prefix, localName, namespaceURI);
	}

	@Override
	public void writeStartElement(String localName) throws XMLStreamException {
		this.out.writeStartElement(localName);
	}

	@Override
	public void writeStartElement(String namespaceURI, String localName) throws XMLStreamException {
		this.out.writeStartElement(namespaceURI, localName);
	}

	@Override
	public void writeStartElement(String prefix, String localName, String namespaceURI) throws XMLStreamException {
		this.out.writeStartElement(prefix, localName, namespaceURI);
	}

	@Override
	public void writeDefaultNamespace(String namespaceURI) throws XMLStreamException {
		this.out.writeDefaultNamespace(namespaceURI);
	}

	@Override
	public void writeNamespace(String prefix, String namespaceURI) throws XMLStreamException {
		this.out.writeNamespace(prefix, namespaceURI);
	}

	@Override
	public String getPrefix(String uri) throws XMLStreamException {
		return this.out.getPrefix(uri);
	}

	@Override
	public void setPrefix(String prefix, String uri) throws XMLStreamException {
		this.out.setPrefix(prefix, uri);
	}

	@Override
	public void writeAttribute(String localName, String value) throws XMLStreamException {
		this.out.writeAttribute(localName, value);
	}

	@Override
	public void writeAttribute(String namespaceURI, String localName, String value) throws XMLStreamException {
		this.out.writeAttribute(namespaceURI, localName, value);
	}

	@Override
	public void writeAttribute(String prefix, String namespaceURI, String localName, String value)
			throws XMLStreamException {
		this.out.writeAttribute(prefix, namespaceURI, localName, value);
	}

	@Override
	public void writeCharacters(String text) throws XMLStreamException {
		this.out.writeCharacters(text);
	}

	@Override
	public void writeCharacters(char[] text, int start, int len) throws XMLStreamException {
		this.out.writeCharacters(text, start, len);
	}

	@Override
	public void writeCData(String data) throws XMLStreamException {
		this.out.writeCData(data);
	}

	@Override
	public void writeEntityRef(String name) throws XMLStreamException {
		this.out.writeEntityRef(name);
	}

	@Override
	public void writeEndElement() throws XMLStreamException {
		this.out.writeEndElement();
	}

	@Override
	public void writeEndDocument() throws XMLStreamException {
		this.out.writeEndDocument();
	}

	@Override
	public void flush() throws XMLStreamException {
		this.out.flush();
	}

	@Override
	public void close() throws XMLStreamException {
		this.out.close();
	}

}
