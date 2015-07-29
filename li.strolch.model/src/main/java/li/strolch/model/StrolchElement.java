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

import java.io.Serializable;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface StrolchElement extends Serializable {

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
	 *            the dbid to set
	 */
	public void setDbid(long dbid);

	/**
	 * Returns the currently set long value which defines the primary key for use in RDBM-Systems
	 *
	 * @return the currently set long value which defines the primary key for use in RDBM-Systems
	 */
	public long getDbid();

	/**
	 * Returns the type of this {@link StrolchElement}
	 *
	 * @return the type of this {@link StrolchElement}
	 */
	public String getType();

	/**
	 * @return the direct parent of this element
	 */
	public StrolchElement getParent();

	/**
	 * Returns the {@link StrolchRootElement} for this {@link StrolchElement}
	 * 
	 * @return the {@link StrolchRootElement} for this {@link StrolchElement}
	 */
	public StrolchRootElement getRootElement();

	/**
	 * Returns true if this element is a {@link StrolchRootElement}, false if not
	 * 
	 * @return true if this element is a {@link StrolchRootElement}, false if not
	 */
	public boolean isRootElement();

	/**
	 * Return a clone of this {@link StrolchElement}
	 *
	 * @return a clone of this {@link StrolchElement}
	 */
	public StrolchElement getClone();

	/**
	 * <p>
	 * Returns the hashcode of this element.
	 * </p>
	 * 
	 * <p>
	 * For most {@link StrolchElement} the equals and hashcode methods would only take into account the ID of the
	 * element, but certain implementations might require more specific attributes
	 * </p>
	 * 
	 * @see Object#hashCode()
	 * 
	 * @return the hashcode of this element
	 */
	@Override
	public int hashCode();

	/**
	 * <p>
	 * Returns true if this object equals the given parameter object
	 * </p>
	 * 
	 * <p>
	 * For most {@link StrolchElement} the equals and hashcode methods would only take into account the ID of the
	 * element, but certain implementations might require more specific attributes
	 * </p>
	 * 
	 * @see Object#equals(Object)
	 * 
	 * @param obj
	 *            the object to which to check for equality
	 * 
	 * @return true if this object equals the given parameter object
	 */
	@Override
	public boolean equals(Object obj);
}
