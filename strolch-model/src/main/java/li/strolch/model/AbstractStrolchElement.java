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

import java.text.MessageFormat;

import li.strolch.exception.StrolchException;
import li.strolch.exception.StrolchModelException;
import li.strolch.model.Locator.LocatorBuilder;
import li.strolch.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractStrolchElement implements StrolchElement {

	protected long dbid = Long.MAX_VALUE;
	protected String id;
	protected String name;
	protected boolean readOnly;

	/**
	 * Empty constructor - for marshalling only!
	 */
	public AbstractStrolchElement() {
		super();
	}

	/**
	 * Default constructor
	 *
	 * @param id   id of this {@link StrolchElement}
	 * @param name name of this {@link StrolchElement}
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
		assertNotReadonly();
		if (StringHelper.isEmpty(id)) {
			String msg = "The id may never be empty for {0}";
			msg = MessageFormat.format(msg, getClass().getSimpleName());
			throw new StrolchException(msg);
		}
		this.id = id;
	}

	@Override
	public String getName() {
		return this.name;
	}

	@Override
	public void setName(String name) {
		assertNotReadonly();
		if (StringHelper.isEmpty(name)) {
			String msg = "The name may never be empty for {0} {1}";
			msg = MessageFormat.format(msg, getClass().getSimpleName(), getLocator());
			throw new StrolchException(msg);
		}
		this.name = name;
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
	protected void fillClone(AbstractStrolchElement clone) {
		clone.id = this.id;
		clone.name = this.name;
	}

	@Override
	public boolean isReadOnly() {
		return this.readOnly;
	}

	@Override
	public void setReadOnly() {
		this.readOnly = true;
	}

	@Override
	public void assertNotReadonly() throws StrolchModelException {
		if (this.readOnly) {
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
		return getLocator().toString();
	}
}
