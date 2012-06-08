/*
 * Copyright (c) 2010 - 2011
 * 
 * Apixxo AG 
 * Hauptgasse 25
 * 4600 Olten
 * 
 * All rights reserved.
 * 
 */
package ch.eitchnet.featherlite.plugin.xmlpers.test.impl;

import org.w3c.dom.DOMImplementation;
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

	/**
	 * @see ch.eitchnet.xmlpers.XmlDao#getType(java.lang.Object)
	 */
	@Override
	public String getType(MyClass object) {
		return MyClass.class.getName();
	}

	/**
	 * @see ch.eitchnet.xmlpers.XmlDao#getSubType(java.lang.Object)
	 */
	@Override
	public String getSubType(MyClass object) {
		return object.getType();
	}

	/**
	 * @see ch.eitchnet.xmlpers.XmlDao#getId(java.lang.Object)
	 */
	@Override
	public String getId(MyClass object) {
		return object.getId();
	}

	/**
	 * @see ch.eitchnet.xmlpers.XmlDao#serializeToDom(java.lang.Object, org.w3c.dom.DOMImplementation)
	 */
	@Override
	public Document serializeToDom(MyClass object, DOMImplementation domImplementation) {

		Document document = domImplementation.createDocument(null, null, null);
		Element element = document.createElement("MyClass");
		document.appendChild(element);

		element.setAttribute("id", object.getId());
		element.setAttribute("type", object.getType());

		Element nameElement = document.createElement("Name");
		element.appendChild(nameElement);
		Text textNode = document.createTextNode(object.getName());
		nameElement.appendChild(textNode);

		return document;
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
