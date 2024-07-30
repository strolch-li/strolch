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

import li.strolch.exception.StrolchModelException;
import li.strolch.model.visitor.StrolchElementVisitor;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface StrolchElement {

	/**
	 * Return the {@link Locator} for this element
	 *
	 * @return the {@link Locator} for this element
	 */
	Locator getLocator();

	/**
	 * Set the semi unique id of this {@link StrolchElement}. This value should be unique for all concrete
	 * implementations of this interface
	 *
	 * @param id
	 */
	void setId(String id);

	/**
	 * Returns the semi unique id of this {@link StrolchElement}. This value should be unique for all concrete
	 * implementations of this interface
	 *
	 * @return
	 */
	String getId();

	/**
	 * Set the name of this {@link StrolchElement}
	 *
	 * @param name
	 */
	void setName(String name);

	/**
	 * Returns the name of this {@link StrolchElement}
	 *
	 * @return
	 */
	String getName();

	/**
	 * Set the currently set long value which defines the primary key for use in RDBM-Systems
	 *
	 * @param dbid
	 * 		the dbid to set
	 */
	void setDbid(long dbid);

	/**
	 * Returns the currently set long value which defines the primary key for use in RDBM-Systems
	 *
	 * @return the currently set long value which defines the primary key for use in RDBM-Systems
	 */
	long getDbid();

	/**
	 * Returns the type of this {@link StrolchElement}
	 *
	 * @return the type of this {@link StrolchElement}
	 */
	String getType();

	/**
	 * @return the direct parent of this element
	 */
	StrolchElement getParent();

	/**
	 * Returns the {@link StrolchRootElement} for this {@link StrolchElement}
	 *
	 * @return the {@link StrolchRootElement} for this {@link StrolchElement}
	 */
	StrolchRootElement getRootElement();

	/**
	 * Returns true if this element is a {@link StrolchRootElement}, false if not
	 *
	 * @return true if this element is a {@link StrolchRootElement}, false if not
	 */
	boolean isRootElement();

	/**
	 * Return a clone of this {@link StrolchElement}
	 *
	 * @return a clone of this {@link StrolchElement}
	 */
	StrolchElement getClone();

	/**
	 * Asserts that this element is not read-only, throwing {@link StrolchModelException} if it is read-only
	 *
	 * @throws StrolchModelException
	 * 		if this element is read-only
	 */
	void assertNotReadonly() throws StrolchModelException;

	/**
	 * Returns true if this element is read only, in which case modifications will throw an exception. To modify it,
	 * call {@link #getClone()}
	 *
	 * @return true if this element is read only
	 */
	boolean isReadOnly();

	/**
	 * Sets this element to readOnly, so that it may not be modified. To modify it, call {@link #getClone()}
	 */
	void setReadOnly();

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
	 * @return the hashcode of this element
	 *
	 * @see Object#hashCode()
	 */
	@Override
	int hashCode();

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
	 * @param obj
	 * 		the object to which to check for equality
	 *
	 * @return true if this object equals the given parameter object
	 *
	 * @see Object#equals(Object)
	 */
	@Override
	boolean equals(Object obj);

	<U> U accept(StrolchElementVisitor<U> visitor);
}
