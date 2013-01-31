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

import li.strolch.exception.StrolchException;
import li.strolch.model.Locator.LocatorBuilder;

import org.w3c.dom.Element;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractStrolchElement implements StrolchElement {

	private static final long serialVersionUID = 0L;

	protected long dbid = Long.MAX_VALUE;
	protected String id;
	protected String name;

	/**
	 * Empty constructor
	 */
	public AbstractStrolchElement() {
		//
	}

	/**
	 * Default constructor
	 * 
	 * @param id
	 *            id of this {@link StrolchElement}
	 * @param name
	 *            name of this {@link StrolchElement}
	 */
	public AbstractStrolchElement(String id, String name) {
		setId(id);
		setName(name);
	}

	@Override
	public long getDbid() {
		return this.dbid;
	}

	@Override
	public void setDbid(long dbid) {
		this.dbid = dbid;
	}

	@Override
	public String getId() {
		return this.id;
	}

	@Override
	public void setId(String id) {
		if (id == null)
			throw new StrolchException("The id may never be null");

		if (id.isEmpty())
			throw new StrolchException("The id may never be empty");

		this.id = id;
	}

	@Override
	public String getName() {
		return this.name;
	}

	@Override
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Used to build a {@link Locator} for this {@link StrolchElement}. It must be implemented by the concrete
	 * implemented as parents must first add their {@link Locator} information
	 * 
	 * @param locatorBuilder
	 *            the {@link LocatorBuilder} to which the {@link StrolchElement} must add its locator information
	 */
	protected abstract void fillLocator(LocatorBuilder locatorBuilder);

	/**
	 * fills the {@link StrolchElement} clone with the id, name and type
	 * 
	 * @param clone
	 */
	protected void fillClone(StrolchElement clone) {
		clone.setId(getId());
		clone.setName(getName());
	}

	protected void fillElement(Element element) {
		element.setAttribute("Id", getId());
		element.setAttribute("Name", getName());
		element.setAttribute("Type", getType());
	}

	/**
	 * Builds the fields of this {@link StrolchElement} from a {@link Element}
	 * 
	 * @param element
	 */
	protected void fromDom(Element element) {
		String id = element.getAttribute("Id");
		String name = element.getAttribute("Name");

		if (id != null && name != null) {
			setId(id);
			setName(name);
		} else {
			throw new StrolchException("Check the values of the element: " + element.getNodeName()
					+ " either id or name attribute is null!");
		}
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.id == null) ? 0 : this.id.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		AbstractStrolchElement other = (AbstractStrolchElement) obj;
		if (this.id == null) {
			if (other.id != null)
				return false;
		} else if (!this.id.equals(other.id))
			return false;
		return true;
	}

	@Override
	public int compareTo(StrolchElement o) {
		return getId().compareTo(o.getId());
	}

	@Override
	public abstract String toString();
}
