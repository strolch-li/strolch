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
package li.strolch.model.xml;

import javax.xml.parsers.DocumentBuilder;

import li.strolch.model.Resource;
import li.strolch.model.ResourceVisitor;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import ch.eitchnet.utils.helper.DomUtil;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ResourceToDomVisitor implements ResourceVisitor<Document> {

	private Document document;

	/**
	 * @return the document
	 */
	public Document getDocument() {
		return this.document;
	}

	@Override
	public Document visit(Resource resource) {
		DocumentBuilder documentBuilder = DomUtil.createDocumentBuilder();
		Document document = documentBuilder.getDOMImplementation().createDocument(null, null, null);

		Element resourceDom = resource.toDom(document);
		document.appendChild(resourceDom);
		this.document = document;
		return this.document;
	}
}
