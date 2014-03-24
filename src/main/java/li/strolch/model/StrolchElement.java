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
	 * @param doc the document to which this element is being written. The client must not append to the document, the
	 * caller will perform this as needed
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

	public StrolchElement getParent();

	public StrolchRootElement getRootElement();

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
