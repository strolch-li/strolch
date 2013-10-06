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
package ch.eitchnet.xmlpers.test.impl;

import java.io.File;

import javax.xml.stream.XMLStreamException;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import ch.eitchnet.xmlpers.api.XmlPersistenceSaxContextData;
import ch.eitchnet.xmlpers.api.XmlPersistenceSaxWriter;
import ch.eitchnet.xmlpers.impl.XmlPersistenceStreamWriter;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class BookSaxDao extends BookDao {

	@Override
	protected Book read(File filePath) {

		XmlPersistenceSaxContextData cd = new XmlPersistenceSaxContextData();
		cd.setFile(filePath);
		BookDefaultHandler bookDefaultHandler = new BookDefaultHandler();
		cd.setDefaultHandler(bookDefaultHandler);

		getFileHandler().read(cd);

		return bookDefaultHandler.getBook();
	}

	@Override
	protected void write(Book book, File filePath) {

		XmlPersistenceSaxContextData cd = new XmlPersistenceSaxContextData();
		cd.setFile(filePath);
		cd.setXmlWriter(new BookSaxWriter(book));

		getFileHandler().write(cd);
	}

	private class BookSaxWriter implements XmlPersistenceSaxWriter {

		private final Book book;

		public BookSaxWriter(Book book) {
			this.book = book;
		}

		@Override
		public void write(XmlPersistenceStreamWriter writer) throws XMLStreamException {
			writer.writeEmptyElement("Book");
			writer.writeAttribute("id", Long.toString(this.book.getId()));
			writer.writeAttribute("title", this.book.getTitle());
			writer.writeAttribute("author", this.book.getAuthor());
			writer.writeAttribute("press", this.book.getPress());
			writer.writeAttribute("price", Double.toString(this.book.getPrice()));
		}
	}

	private class BookDefaultHandler extends DefaultHandler {

		private Book book;

		public BookDefaultHandler() {
			// default constructor
		}

		public Book getBook() {
			return this.book;
		}

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
}