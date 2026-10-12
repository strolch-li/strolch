/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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
import li.strolch.model.Locator.LocatorBuilder;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractStrolchElement implements StrolchElement {

	/**
	 * Empty constructor - for marshalling only!
	 */
	protected AbstractStrolchElement() {
		super();
	}

	/**
	 * Default constructor
	 *
	 * @param id   id of this {@link StrolchElement}
	 * @param name name of this {@link StrolchElement}
	 */
	protected AbstractStrolchElement(String id, String name) {
		setId(id);
		setName(name);
	}

	/**
	 * Used to build a {@link Locator} for this {@link StrolchElement}. It must be implemented by the concrete
	 * implemented as parents must first add their {@link Locator} information
	 *
	 * @param locatorBuilder the {@link LocatorBuilder} to which the {@link StrolchElement} must add its locator
	 *                       information
	 */
	protected abstract void fillLocator(LocatorBuilder locatorBuilder);

	/**
	 * fills the {@link StrolchElement} clone with the id, name and type
	 *
	 * @param clone the clone to fill
	 */
	protected abstract void fillClone(AbstractStrolchElement clone);

	@Override
	public void assertNotReadonly() throws StrolchModelException {
		if (isReadOnly()) {
			throw new StrolchModelException(
					"The element " + getLocator() + " is currently readOnly, to modify clone first!");
		}
	}

	@Override
	public abstract boolean equals(Object obj);

	@Override
	public abstract int hashCode();

	@Override
	public String toString() {
		return getLocator() + ", Name: " + getName();
	}
}
