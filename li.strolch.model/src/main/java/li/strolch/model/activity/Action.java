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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.Locator;
import li.strolch.model.Locator.LocatorBuilder;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.StrolchElement;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.model.timevalue.IValueChange;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * An {@link Action} represents a single step within an {@link Activity}, that
 * is, one that is not further decomposed within the {@link Activity}. A
 * {@link Activity} applies {@link IValueChange} objects at the start and end
 * time of the {@link Activity}.
 * 
 * @author Martin Smock <martin.smock@bluewin.ch>
 */
public class Action extends GroupedParameterizedElement implements IActivityElement {

	protected static final long serialVersionUID = 1L;

	protected Activity parent;
	protected String resourceId, resourceType;
	protected State state = State.CREATED;

	protected final List<IValueChange<?>> changes = new ArrayList<>();

	public Action(String id, String name, String type) {
		super(id, name, type);
	}

	/**
	 * @return the id of the {@link Resource} the {@link Action} acts on
	 */
	public String getResourceId() {
		return resourceId;
	}

	/**
	 * @param resourceId
	 *            the id of the {@link Resource} the {@link Action} acts on
	 */
	public void setResourceId(String resourceId) {
		this.resourceId = resourceId;
	}

	/**
	 * @return the current <code>State</code> of the a<code>Action</code>
	 */
	public State getState() {
		return state;
	}

	/**
	 * @param state
	 *            the target <code>State</code> of the a<code>Action</code>
	 */
	public void setState(State state) {
		this.state = state;
	}

	/**
	 * @return the type of the <code>Resource</code> this <code>Action</code>
	 *         acts on
	 */
	public String getResourceType() {
		return this.resourceType;
	}

	/**
	 * @param resourceType
	 */
	public void setResourceType(String resourceType) {
		this.resourceType = resourceType;
	}

	/**
	 * @param add
	 *            <code>IValueChange</code> to be applied to the
	 *            <code>Resource</code>
	 * 
	 * @return <tt>true</tt> (as specified by {@link Collection#add})
	 */
	public boolean addChange(IValueChange<?> change) {
		return changes.add(change);
	}

	/**
	 * @return the list of <code>IValueChange</code> attached to the
	 *         <code>Action</code> start
	 */
	public List<IValueChange<?>> getChanges() {
		return changes;
	}

	@Override
	public StrolchElement getParent() {
		return parent;
	}

	@Override
	public StrolchRootElement getRootElement() {
		return (parent == null) ? null : parent.getRootElement();
	}

	@Override
	public boolean isRootElement() {
		return false;
	}

	@Override
	public StrolchElement getClone() {
		Action clone = new Action(getId(), getName(), getType());
		clone.setDbid(getDbid());
		clone.setResourceId(resourceId);
		clone.setResourceType(resourceType);
		clone.setState(state);
		for (IValueChange<?> change : getChanges()) {
			clone.changes.add(change.getClone());
		}
		return clone;
	}

	@Override
	public Locator getLocator() {
		LocatorBuilder lb = new LocatorBuilder();
		this.parent.fillLocator(lb);
		fillLocator(lb);
		return lb.build();
	}

	@Override
	protected void fillLocator(LocatorBuilder locatorBuilder) {
		locatorBuilder.append(this.id);
	}

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("Action [id=");
		builder.append(this.id);
		builder.append(", name=");
		builder.append(this.name);
		builder.append(", type=");
		builder.append(this.type);
		builder.append(", resourceId=");
		builder.append(this.resourceId);
		builder.append(", state=");
		builder.append(this.state);
		builder.append("]");
		return builder.toString();
	}

	@Override
	public void setParent(Activity activity) {
		this.parent = activity;
	}

	@Override
	public Long getStart() {
		Long start = Long.MAX_VALUE;
		for (IValueChange<?> change : changes) {
			start = Math.min(start, change.getTime());
		}
		return start;
	}

	@Override
	public Long getEnd() {
		Long end = 0L;
		for (IValueChange<?> change : changes) {
			end = Math.max(end, change.getTime());
		}
		return end;
	}

	@Override
	public Element toDom(Document doc) {
		Element element = doc.createElement(Tags.ACTION);
		fillElement(element);
		element.setAttribute(Tags.STATE, this.state.toString());
		element.setAttribute(Tags.RESOURCE_ID, this.resourceId);
		element.setAttribute(Tags.RESOURCE_TYPE, this.resourceType);
		for (IValueChange<?> change : changes) {
			element.appendChild(change.toDom(doc));
		}
		return element;
	}

}
