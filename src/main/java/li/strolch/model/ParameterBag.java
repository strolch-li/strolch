/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  li.strolch.model is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  li.strolch.model is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with li.strolch.model.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.model;

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
	public Element toDom(Document doc) {

		Element element = doc.createElement("ParameterBag");

		fillElement(element);

		return element;
	}
}
