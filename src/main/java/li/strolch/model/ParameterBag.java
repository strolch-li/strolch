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

import li.strolch.model.Locator.LocatorBuilder;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ParameterBag extends ParameterizedElement {

	private static final long serialVersionUID = 1L;

	/**
	 * Empty Constructor
	 */
	public ParameterBag() {
		// 
	}

	/**
	 * Default constructor
	 *
	 * @param id
	 * @param name
	 * @param type
	 */
	public ParameterBag(String id, String name, String type) {
		super(id, name, type);
	}

	/**
	 * DOM Constructor
	 *
	 * @param bagElement
	 */
	public ParameterBag(Element bagElement) {
		super.fromDom(bagElement);
	}

	@Override
	public ParameterBag getClone() {
		ParameterBag clone = new ParameterBag();
		super.fillClone(clone);
		return clone;
	}

	@Override
	public void fillLocator(LocatorBuilder lb) {
		this.parent.fillLocator(lb);
		lb.append(Tags.BAG);
		lb.append(this.id);
	}

	@Override
	public Element toDom(Document doc) {

		Element element = doc.createElement(Tags.PARAMETER_BAG);

		fillElement(element);

		return element;
	}
}
