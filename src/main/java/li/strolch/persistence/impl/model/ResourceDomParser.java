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
package li.strolch.persistence.impl.model;

import javax.xml.parsers.DocumentBuilder;

import li.strolch.model.Resource;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import ch.eitchnet.xmlpers.api.DomParser;
import ch.eitchnet.xmlpers.util.DomUtil;

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

		Element resourceDom = this.resource.toDom(document);
		document.appendChild(resourceDom);

		return document;
	}

	@Override
	public void fromDom(Document document) {

		Element rootElement = document.getDocumentElement();
		Resource resource = new Resource(rootElement);
		this.resource = resource;
	}
}