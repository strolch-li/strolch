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

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import ch.eitchnet.xmlpers.test.model.Book;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
@SuppressWarnings("nls")
public class BookDomDao {

	public Element serializeToDom(Book book, Document document) {

		Element element = document.createElement("Book");
		document.appendChild(element);
		element.setAttribute("id", Long.toString(book.getId()));
		element.setAttribute("title", book.getTitle());
		element.setAttribute("author", book.getAuthor());
		element.setAttribute("press", book.getPress());
		element.setAttribute("price", Double.toString(book.getPrice()));

		return element;
	}

	public Book parseFromDom(Element element) {

		String idS = element.getAttribute("id");
		long id = Long.parseLong(idS);
		String title = element.getAttribute("title");
		String author = element.getAttribute("author");
		String press = element.getAttribute("press");
		String priceS = element.getAttribute("price");
		double price = Double.parseDouble(priceS);

		Book book = new Book(id, title, author, press, price);
		return book;
	}
}
