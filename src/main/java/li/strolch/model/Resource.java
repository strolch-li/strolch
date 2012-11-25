/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of li.strolch.model.
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

import li.strolch.model.Locator.LocatorBuilder;

import org.dom4j.Element;
import org.dom4j.tree.DefaultElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class Resource extends GroupedParameterizedElement {

	private static final long serialVersionUID = 0L;
	public static final String PREFIX_RESOURCE = "ResourcePrefix";

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
	public Element toDom() {

		Element element = new DefaultElement("Resource");
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
		lb.append("Resource").append(getId());
	}

	@Override
	public Locator getLocator() {
		LocatorBuilder lb = new LocatorBuilder();
		fillLocator(lb);
		return lb.build();
	}

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
