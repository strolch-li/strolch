/*
 * Copyright 2015 Martin Smock <martin.smock@bluewin.ch>
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
package li.strolch.model.activity;

import li.strolch.exception.StrolchModelException;
import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.State;
import li.strolch.model.StrolchElement;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.visitor.IActivityElementVisitor;

/**
 * Marker for all child elements of {@link Activity} objects
 * 
 * @author Martin Smock <martin.smock@bluewin.ch>
 */
public interface IActivityElement extends StrolchElement {

	/**
	 * @return the start time of this element
	 */
	public Long getStart();

	/**
	 * @return the end time of this element
	 */
	public Long getEnd();

	/**
	 * @return the {@link State} of this element
	 */
	public State getState();

	/**
	 * Set the parent
	 * 
	 * @param activity
	 */
	public void setParent(Activity activity);

	/**
	 * <p>
	 * Checks if this element contains the {@link Parameter}, or otherwise queries its parent, until the root element is
	 * reached.
	 * </p>
	 * 
	 * <p>
	 * If the parameter does not exist, null is returned
	 * </p>
	 * 
	 * @see GroupedParameterizedElement#getParameter(String, String)
	 */
	public <T extends Parameter<?>> T findParameter(String bagKey, String paramKey);

	/**
	 * <p>
	 * Checks if this element contains the {@link Parameter}, or otherwise queries its parent, until the root element is
	 * reached.
	 * </p>
	 * 
	 * <p>
	 * If the parameter does not exist and <code>assertExists</code> is true, then an
	 * </p>
	 * 
	 * @see GroupedParameterizedElement#getParameter(String, String, boolean)
	 */
	public <T extends Parameter<?>> T findParameter(String bagKey, String paramKey, boolean assertExists)
			throws StrolchModelException;

	@Override
	public Activity getParent();

	@Override
	public Activity getRootElement();

	@Override
	public IActivityElement getClone();

	/**
	 * Implements the visitor pattern. Concrete implementation will call the proper method on the visitor
	 * 
	 * @param visitor
	 *            the visitor to accept
	 * 
	 * @return the result of the visitor being accepted
	 */
	public <T> T accept(IActivityElementVisitor<T> visitor);
}
