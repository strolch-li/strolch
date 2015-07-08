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
import javax.xml.stream.XMLStreamWriter;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import ch.eitchnet.xmlpers.api.SaxParser;
import ch.eitchnet.xmlpers.test.model.Book;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class BookSaxParser extends DefaultHandler implements SaxParser<Book> {

	private Book book;

	@Override
	public Book getObject() {
		return this.book;
	}

	@Override
	public void setObject(Book object) {
		this.book = object;

	}

	@Override
	public DefaultHandler getDefaultHandler() {
		return this;
	}

	@SuppressWarnings("nls")
	@Override
	public void write(XMLStreamWriter writer) throws XMLStreamException {

		writer.writeEmptyElement("Book");
		writer.writeAttribute("id", Long.toString(this.book.getId()));
		writer.writeAttribute("title", this.book.getTitle());
		writer.writeAttribute("author", this.book.getAuthor());
		writer.writeAttribute("press", this.book.getPress());
		writer.writeAttribute("price", Double.toString(this.book.getPrice()));
	}

	@SuppressWarnings("nls")
	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {

		switch (qName) {
		case "Book":
			String idS = attributes.getValue("id");
			long id = Long.parseLong(idS);
			Book book = new Book(id);
			book.setTitle(attributes.getValue("title"));
			book.setAuthor(attributes.getValue("author"));
			book.setPress(attributes.getValue("press"));
			String priceS = attributes.getValue("price");
			double price = Double.parseDouble(priceS);
			book.setPrice(price);
			this.book = book;
			break;
		default:
			throw new IllegalArgumentException("The element '" + qName + "' is unhandled!");
		}
	}
}
