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

import java.io.Serializable;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface StrolchElement extends Serializable, Comparable<StrolchElement> {

	/**
	 * Return the {@link Locator} for this element
	 * 
	 * @return the {@link Locator} for this element
	 */
	public Locator getLocator();

	/**
	 * Set the semi unique id of this {@link StrolchElement}. This value should be unique under all concrete
	 * implementations of this interface
	 * 
	 * @param id
	 */
	public void setId(String id);

	/**
	 * Returns the semi unique id of this {@link StrolchElement}. This value should be unique under all concrete
	 * implementations of this interface
	 * 
	 * @return
	 */
	public String getId();

	/**
	 * Set the name of this {@link StrolchElement}
	 * 
	 * @param name
	 */
	public void setName(String name);

	/**
	 * Returns the name of this {@link StrolchElement}
	 * 
	 * @return
	 */
	public String getName();

	/**
	 * Set the currently set long value which defines the primary key for use in RDBM-Systems
	 * 
	 * @param dbid
	 */
	public void setDbid(long dbid);

	/**
	 * Returns the currently set long value which defines the primary key for use in RDBM-Systems
	 * 
	 * @return
	 */
	public long getDbid();

	/**
	 * Returns an {@link Element} object which is an XML representation of this object
	 * 
	 * @param doc
	 *            the document to which this element is being written. The client should not append to the document, the
	 *            caller will perform this as needed
	 * 
	 * @return
	 */
	public Element toDom(Document doc);

	/**
	 * Returns the type of this {@link StrolchElement}
	 * 
	 * @return
	 */
	public String getType();

	/**
	 * Return a clone of this {@link StrolchElement}
	 * 
	 * @return
	 */
	public StrolchElement getClone();

	@Override
	public int hashCode();

	@Override
	public boolean equals(Object obj);

	@Override
	public int compareTo(StrolchElement o);
}
