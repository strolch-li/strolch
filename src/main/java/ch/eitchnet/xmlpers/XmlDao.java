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
package ch.eitchnet.xmlpers;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.ContentHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public interface XmlDao<T> {

	/**
	 * @param object
	 * @return
	 */
	public String getType(T object);

	/**
	 * @param object
	 * @return
	 */
	public String getSubType(T object);

	/**
	 * @param object
	 * @return
	 */
	public String getId(T object);

	/**
	 * 
	 * @param object
	 * @param document
	 * @return
	 */
	public Element serializeToDom(T object, Document document);

	/**
	 * @param element
	 * @return
	 */
	public T parseFromDom(Element element);

	/**
	 * @param object
	 * @param contentHandler
	 */
	// XXX Use the XMLSerializer object for serializing to SAX...
	public void serializeToSax(T object, ContentHandler contentHandler);

	// XXX parse from SAX is missing...

}
