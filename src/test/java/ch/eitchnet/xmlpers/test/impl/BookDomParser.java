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

import javax.xml.parsers.DocumentBuilder;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import ch.eitchnet.xmlpers.api.DomParser;
import ch.eitchnet.xmlpers.test.model.Book;
import ch.eitchnet.xmlpers.util.DomUtil;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class BookDomParser implements DomParser<Book> {

	private Book book;

	@Override
	public Book getObject() {
		return this.book;
	}

	@Override
	public void setObject(Book object) {
		this.book = object;
	}

	@SuppressWarnings("nls")
	@Override
	public Document toDom() {

		DocumentBuilder documentBuilder = DomUtil.createDocumentBuilder();
		Document document = documentBuilder.getDOMImplementation().createDocument(null, null, null);

		Element rootElement = document.createElement("Book");
		document.appendChild(rootElement);
		rootElement.setAttribute("id", Long.toString(this.book.getId()));
		rootElement.setAttribute("title", this.book.getTitle());
		rootElement.setAttribute("author", this.book.getAuthor());
		rootElement.setAttribute("press", this.book.getPress());
		rootElement.setAttribute("price", Double.toString(this.book.getPrice()));

		return document;

	}

	@SuppressWarnings("nls")
	@Override
	public void fromDom(Document document) {

		Element rootElement = document.getDocumentElement();

		String idS = rootElement.getAttribute("id");
		long id = Long.parseLong(idS);
		String title = rootElement.getAttribute("title");
		String author = rootElement.getAttribute("author");
		String press = rootElement.getAttribute("press");
		String priceS = rootElement.getAttribute("price");
		double price = Double.parseDouble(priceS);

		Book book = new Book(id, title, author, press, price);
		this.book = book;
	}
}
