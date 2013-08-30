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

import java.io.File;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Text;

import ch.eitchnet.xmlpers.test.model.Resource;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public abstract class ResourceDomDao extends ResourceDao {

	@Override
	protected Resource read(File filePath) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected void write(Resource object, File filePath) {
		// TODO Auto-generated method stub

	}

	public Element serializeToDom(Resource object, Document document) {

		Element element = document.createElement("Resource");

		element.setAttribute("id", object.getId());
		element.setAttribute("type", object.getType());

		Element nameElement = document.createElement("Name");
		element.appendChild(nameElement);
		Text textNode = document.createTextNode(object.getName());
		nameElement.appendChild(textNode);

		return element;
	}

	public Resource parseFromDom(Element element) {

		String id = element.getAttribute("id");
		String type = element.getAttribute("type");
		Element nameElement = (Element) element.getElementsByTagName("Name").item(0);
		String name = nameElement.getTextContent();

		Resource Resource = new Resource(id, name, type);

		return Resource;
	}
}
