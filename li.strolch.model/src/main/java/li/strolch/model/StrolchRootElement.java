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

import li.strolch.model.visitor.StrolchRootElementVisitor;

/**
 * Root element for all top level {@link StrolchElement}. These are elements which have no parent, e.g. {@link Resource
 * Resources} and {@link Order Orders}. Every root element has a version, so that versions can be kept of an object
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface StrolchRootElement extends StrolchElement, PolicyContainer, ParameterBagContainer {

	/**
	 * Returns true if this {@link StrolchRootElement} has a version set
	 * 
	 * @return true if this {@link StrolchRootElement} has a version set
	 */
	public boolean hasVersion();

	/**
	 * Returns the current version of this object, or null if no version is set
	 * 
	 * @return the current version of this object, or null if no version is set
	 */
	public Version getVersion();

	/**
	 * <p>
	 * Sets the version of this object
	 * </p>
	 * 
	 * @param version
	 *            the version to set
	 * 
	 * @throws IllegalArgumentException
	 *             if the given version's locator is not equal to the current element's locator
	 */
	public void setVersion(Version version) throws IllegalArgumentException;

	/**
	 * Visitor pattern accept method. Takes a {@link StrolchRootElementVisitor} to visit this element
	 * 
	 * @param visitor
	 *            the visitor
	 * 
	 * @return the result of the visitation
	 */
	public <T> T accept(StrolchRootElementVisitor<T> visitor);
}
