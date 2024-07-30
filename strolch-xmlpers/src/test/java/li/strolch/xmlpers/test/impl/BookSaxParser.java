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
import li.strolch.xmlpers.test.model.Book;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
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

	@Override
	public void write(XMLStreamWriter writer) throws XMLStreamException {

		writer.writeEmptyElement("Book");
		writer.writeAttribute("id", Long.toString(this.book.getId()));
		writer.writeAttribute("title", this.book.getTitle());
		writer.writeAttribute("author", this.book.getAuthor());
		writer.writeAttribute("press", this.book.getPress());
		writer.writeAttribute("price", Double.toString(this.book.getPrice()));
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) {

		if (qName.equals("Book")) {
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
		} else {
			throw new IllegalArgumentException("The element '" + qName + "' is unhandled!");
		}
	}
}
