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

import javax.xml.parsers.DocumentBuilder;

import li.strolch.xmlpers.api.DomParser;
import li.strolch.xmlpers.test.model.Book;
import li.strolch.xmlpers.util.DomUtil;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
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
