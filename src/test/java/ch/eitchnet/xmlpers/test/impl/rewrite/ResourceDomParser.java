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
package ch.eitchnet.xmlpers.test.impl.rewrite;

import javax.xml.parsers.DocumentBuilder;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import ch.eitchnet.xmlpers.api.DomUtil;
import ch.eitchnet.xmlpers.test.model.Parameter;
import ch.eitchnet.xmlpers.test.model.Resource;

public class ResourceDomParser implements DomParser<Resource> {

	private Resource resource;

	@Override
	public Resource getObject() {
		return this.resource;
	}

	@Override
	public void setObject(Resource resource) {
		this.resource = resource;
	}

	@Override
	public Document toDom() {

		DocumentBuilder documentBuilder = DomUtil.createDocumentBuilder();
		Document document = documentBuilder.getDOMImplementation().createDocument(null, null, null);

		Element element = document.createElement("Resource");
		document.appendChild(element);

		element.setAttribute("id", this.resource.getId());
		element.setAttribute("name", this.resource.getName());
		element.setAttribute("type", this.resource.getType());

		for (String paramId : this.resource.getParameterKeySet()) {
			Parameter param = this.resource.getParameterBy(paramId);
			Element paramElement = document.createElement("Parameter");
			element.appendChild(paramElement);

			paramElement.setAttribute("id", param.getId());
			paramElement.setAttribute("name", param.getName());
			paramElement.setAttribute("type", param.getType());
			paramElement.setAttribute("value", param.getValue());
		}

		return document;
	}

	@Override
	public void fromDom(Document document) {

		Element rootElement = document.getDocumentElement();

		String id = rootElement.getAttribute("id");
		String name = rootElement.getAttribute("name");
		String type = rootElement.getAttribute("type");

		Resource resource = new Resource(id, name, type);

		NodeList children = rootElement.getChildNodes();
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

		this.resource = resource;
	}
}