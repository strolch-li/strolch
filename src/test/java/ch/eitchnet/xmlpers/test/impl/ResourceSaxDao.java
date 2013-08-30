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

import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.AttributesImpl;

import ch.eitchnet.xmlpers.test.model.Resource;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class ResourceSaxDao extends ResourceDao {

	public void serializeToSax(Resource object, ContentHandler contentHandler) {

		try {
			contentHandler.startDocument();

			// Resource element / root
			{
				AttributesImpl atts = new AttributesImpl();
				atts.addAttribute("", "", "id", "", object.getId());
				atts.addAttribute("", "", "type", "", object.getType());
				contentHandler.startElement("", "", "Resource", atts);

				// name element
				{
					contentHandler.startElement("", "", "Name", null);
					char[] nameArr = object.getName().toCharArray();
					contentHandler.characters(nameArr, 0, nameArr.length);
					contentHandler.endElement("", "", "name");
				}

				// Resource end
				contentHandler.endElement("", "", "Resource");
			}

			// end document
			contentHandler.endDocument();

		} catch (SAXException e) {
			throw new RuntimeException("Failed to serialize " + object + " to SAX", e);
		}
	}
}
