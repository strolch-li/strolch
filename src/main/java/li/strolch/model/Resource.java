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
package li.strolch.model;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import li.strolch.model.Locator.LocatorBuilder;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class Resource extends GroupedParameterizedElement {

	private static final long serialVersionUID = 0L;

	/**
	 * Empty constructor
	 */
	public Resource() {
		//
	}

	/**
	 * Default constructor
	 * 
	 * @param id
	 * @param name
	 * @param type
	 */
	public Resource(String id, String name, String type) {
		super(id, name, type);
	}

	/**
	 * DOM Constructor
	 * 
	 * @param element
	 */
	public Resource(Element element) {
		super.fromDom(element);
	}

	@Override
	public Element toDom(Document doc) {

		Element element = doc.createElement(Tags.RESOURCE);
		fillElement(element);

		return element;
	}

	@Override
	public Resource getClone() {
		Resource clone = new Resource();

		super.fillClone(clone);

		return clone;
	}

	@Override
	protected void fillLocator(LocatorBuilder lb) {
		lb.append(Tags.RESOURCE).append(getType()).append(getId());
	}

	@Override
	public Locator getLocator() {
		LocatorBuilder lb = new LocatorBuilder();
		fillLocator(lb);
		return lb.build();
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {

		StringBuilder builder = new StringBuilder();

		builder.append("Resource [id=");
		builder.append(this.id);
		builder.append(", name=");
		builder.append(this.name);
		builder.append(", type=");
		builder.append(this.type);
		builder.append("]");

		return builder.toString();
	}
}
