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
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import li.strolch.exception.StrolchPolicyException;
import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.Locator;
import li.strolch.model.Locator.LocatorBuilder;
import li.strolch.model.PolicyContainer;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.policy.PolicyDefs;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;

/**
 * An {@link Action} represents a single step within an {@link Activity}, that is, one that is not further decomposed
 * within the {@link Activity}. A {@link Activity} applies {@link IValueChange} objects at the start and end time of the
 * {@link Activity}.
 * 
 * @author Martin Smock <martin.smock@bluewin.ch>
 */
public class Action extends GroupedParameterizedElement implements IActivityElement, PolicyContainer {

	protected static final long serialVersionUID = 1L;

	protected Activity parent;
	protected String resourceId;
	protected String resourceType;
	protected State state;
	protected PolicyDefs policyDefs;

	protected List<IValueChange<? extends IValue<?>>> changes;

	/**
	 * Empty constructor - for marshalling only!
	 */
	public Action() {
		super();
	}

	public Action(String id, String name, String type) {
		super(id, name, type);
		this.state = State.CREATED;
	}

	public Action(String id, String name, String type, String resourceId, String resourceType) {
		super(id, name, type);
		this.resourceId = resourceId;
		this.resourceType = resourceType;
		this.state = State.CREATED;
	}

	private void initChanges() {
		if (this.changes == null)
			this.changes = new ArrayList<>();
	}

	/**
	 * @return the id of the {@link Resource} the {@link Action} acts on
	 */
	public String getResourceId() {
		return this.resourceId;
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
	@Override
	public State getState() {
		return this.state;
	}

	/**
	 * @param state
	 *            the target <code>State</code> of the a<code>Action</code>
	 */
	public void setState(State state) {
		this.state = state;
	}

	/**
	 * @return the type of the <code>Resource</code> this <code>Action</code> acts on
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
	 * Returns true if this {@link Action} contains any {@link IValueChange changes}, false if not
	 * 
	 * @return true if this {@link Action} contains any {@link IValueChange changes}, false if not
	 */
	public boolean hasChanges() {
		return this.changes != null && !this.changes.isEmpty();
	}

	/**
	 * @param add
	 *            <code>IValueChange</code> to be applied to the <code>Resource</code>
	 * 
	 * @return <tt>true</tt> (as specified by {@link Collection#add})
	 */
	public boolean addChange(IValueChange<? extends IValue<?>> change) {
		initChanges();
		return this.changes.add(change);
	}

	/**
	 * @return the list of <code>IValueChange</code> attached to the <code>Action</code> start
	 */
	public List<IValueChange<? extends IValue<?>>> getChanges() {
		if (this.changes == null)
			return Collections.emptyList();
		return this.changes;
	}

	public Iterator<IValueChange<? extends IValue<?>>> changesIterator() {
		if (this.changes == null)
			return Collections.<IValueChange<? extends IValue<?>>> emptyList().iterator();
		return this.changes.iterator();
	}

	@Override
	public Activity getParent() {
		return this.parent;
	}

	@Override
	public StrolchRootElement getRootElement() {
		return (this.parent == null) ? null : this.parent.getRootElement();
	}

	@Override
	public boolean isRootElement() {
		return false;
	}

	@Override
	public Action getClone() {
		Action clone = new Action();
		super.fillClone(clone);

		clone.setResourceId(this.resourceId);
		clone.setResourceType(this.resourceType);
		clone.setState(this.state);

		if (this.changes != null) {
			for (IValueChange<? extends IValue<?>> change : getChanges()) {
				clone.addChange(change.getClone());
			}
		}

		return clone;
	}

	@Override
	public PolicyDefs getPolicyDefs() {
		if (this.policyDefs == null)
			throw new StrolchPolicyException(getLocator() + " has no Policies defined!");
		return this.policyDefs;
	}

	@Override
	public boolean hasPolicyDefs() {
		return this.policyDefs != null;
	}

	@Override
	public void setPolicyDefs(PolicyDefs policyDefs) {
		this.policyDefs = policyDefs;
		this.policyDefs.setParent(this);
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
		if (this.changes == null)
			return start;
		for (IValueChange<?> change : this.changes) {
			start = Math.min(start, change.getTime());
		}
		return start;
	}

	@Override
	public Long getEnd() {
		Long end = 0L;
		if (this.changes == null)
			return end;
		for (IValueChange<?> change : this.changes) {
			end = Math.max(end, change.getTime());
		}
		return end;
	}
}
