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
import li.strolch.xmlpers.test.model.MyModel;
import li.strolch.xmlpers.test.model.MyParameter;
import li.strolch.xmlpers.util.DomUtil;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class MyModelDomParser implements DomParser<MyModel> {

	private MyModel resource;

	@Override
	public MyModel getObject() {
		return this.resource;
	}

	@Override
	public void setObject(MyModel resource) {
		this.resource = resource;
	}

	@SuppressWarnings("nls")
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
			MyParameter param = this.resource.getParameterBy(paramId);
			Element paramElement = document.createElement("Parameter");
			element.appendChild(paramElement);

			paramElement.setAttribute("id", param.getId());
			paramElement.setAttribute("name", param.getName());
			paramElement.setAttribute("type", param.getType());
			paramElement.setAttribute("value", param.getValue());
		}

		return document;
	}

	@SuppressWarnings("nls")
	@Override
	public void fromDom(Document document) {

		Element rootElement = document.getDocumentElement();

		String id = rootElement.getAttribute("id");
		String name = rootElement.getAttribute("name");
		String type = rootElement.getAttribute("type");

		MyModel resource = new MyModel(id, name, type);

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

			MyParameter param = new MyParameter(paramId, paramName, paramType, paramValue);
			resource.addParameter(param);
		}

		this.resource = resource;
	}
}