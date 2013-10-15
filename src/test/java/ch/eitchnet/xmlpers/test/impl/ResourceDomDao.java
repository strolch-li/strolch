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
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import ch.eitchnet.xmlpers.test.model.Parameter;
import ch.eitchnet.xmlpers.test.model.Resource;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
@SuppressWarnings("nls")
public class ResourceDomDao {

	public Element serializeToDom(Resource resource, Document document) {

		Element element = document.createElement("Resource");
		document.appendChild(element);

		element.setAttribute("id", resource.getId());
		element.setAttribute("name", resource.getName());
		element.setAttribute("type", resource.getType());

		for (String paramId : resource.getParameterKeySet()) {
			Parameter param = resource.getParameterBy(paramId);
			Element paramElement = document.createElement("Parameter");
			element.appendChild(paramElement);

			paramElement.setAttribute("id", param.getId());
			paramElement.setAttribute("name", param.getName());
			paramElement.setAttribute("type", param.getType());
			paramElement.setAttribute("value", param.getValue());
		}

		return element;
	}

	public Resource parseFromDom(Element element) {

		String id = element.getAttribute("id");
		String name = element.getAttribute("name");
		String type = element.getAttribute("type");

		Resource resource = new Resource(id, name, type);

		NodeList children = element.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node item = children.item(i);
			if (!item.getNodeName().equals("Parameter"))
				continue;

			Element paramElement = (Element) item;
			String paramId = paramElement.getAttribute("id");
			String paramName = paramElement.getAttribute("name");
			String paramType = paramElement.getAttribute("type");
			String paramValue = paramElement.getAttribute("value");

			Parameter param = new Parameter(paramId, paramName, paramType, paramValue);
			resource.addParameter(param);
		}

		return resource;
	}
}
