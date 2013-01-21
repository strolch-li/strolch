/*
 * Copyright (c) 2012
 * 
 * This file is part of ch.eitchnet.java.xmlpers
 *
 * ch.eitchnet.java.xmlpers is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * ch.eitchnet.java.xmlpers is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ch.eitchnet.java.xmlpers.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.xmlpers.test.impl;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Text;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.AttributesImpl;

import ch.eitchnet.xmlpers.XmlDao;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class MyClassDao implements XmlDao<MyClass> {

	@Override
	public String getType(MyClass object) {
		return MyClass.class.getName();
	}

	@Override
	public String getSubType(MyClass object) {
		return object.getType();
	}

	@Override
	public String getId(MyClass object) {
		return object.getId();
	}

	@Override
	public Element serializeToDom(MyClass object, Document document) {

		Element element = document.createElement("MyClass");

		element.setAttribute("id", object.getId());
		element.setAttribute("type", object.getType());

		Element nameElement = document.createElement("Name");
		element.appendChild(nameElement);
		Text textNode = document.createTextNode(object.getName());
		nameElement.appendChild(textNode);

		return element;
	}

	/**
	 * @see ch.eitchnet.xmlpers.XmlDao#parseFromDom(org.w3c.dom.Element)
	 */
	@Override
	public MyClass parseFromDom(Element element) {

		String id = element.getAttribute("id");
		String type = element.getAttribute("type");
		Element nameElement = (Element) element.getElementsByTagName("Name").item(0);
		String name = nameElement.getTextContent();

		MyClass myClass = new MyClass(id, name, type);

		return myClass;
	}

	/**
	 * @see ch.eitchnet.xmlpers.XmlDao#serializeToSax(java.lang.Object, org.xml.sax.ContentHandler)
	 */
	@Override
	public void serializeToSax(MyClass object, ContentHandler contentHandler) {

		try {
			contentHandler.startDocument();

			// MyClass element / root
			{
				AttributesImpl atts = new AttributesImpl();
				atts.addAttribute("", "", "id", "", object.getId());
				atts.addAttribute("", "", "type", "", object.getType());
				contentHandler.startElement("", "", "MyClass", atts);

				// name element
				{
					contentHandler.startElement("", "", "Name", null);
					char[] nameArr = object.getName().toCharArray();
					contentHandler.characters(nameArr, 0, nameArr.length);
					contentHandler.endElement("", "", "name");
				}

				// MyClass end
				contentHandler.endElement("", "", "MyClass");
			}

			// end document
			contentHandler.endDocument();

		} catch (SAXException e) {
			throw new RuntimeException("Failed to serialize " + object + " to SAX", e);
		}
	}
}
