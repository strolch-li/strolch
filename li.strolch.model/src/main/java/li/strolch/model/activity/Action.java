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

import static li.strolch.model.StrolchModelConstants.BAG_RELATIONS;
import static li.strolch.utils.helper.StringHelper.isNotEmpty;

import java.text.MessageFormat;
import java.util.*;

import li.strolch.exception.StrolchModelException;
import li.strolch.exception.StrolchPolicyException;
import li.strolch.model.*;
import li.strolch.model.Locator.LocatorBuilder;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.policy.PolicyDefs;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.visitor.StrolchElementVisitor;
import li.strolch.utils.dbc.DBC;

/**
 * An {@link Action} represents a single step within an {@link Activity}, that is, one that is not further decomposed
 * within the {@link Activity}. A {@link Activity} applies {@link IValueChange} objects at the start and end time of the
 * {@link Activity}.
 *
 * @author Martin Smock <martin.smock@bluewin.ch>
 */
public class Action extends GroupedParameterizedElement implements IActivityElement, PolicyContainer {

	protected Locator locator;
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

	@Override
	public boolean isAction() {
		return true;
	}

	@Override
	public boolean isActivity() {
		return false;
	}

	/**
	 * @return the id of the {@link Resource} the {@link Action} acts on
	 */
	public String getResourceId() {
		return this.resourceId;
	}

	/**
	 * @param resourceId
	 * 		the id of the {@link Resource} the {@link Action} acts on
	 */
	public void setResourceId(String resourceId) {
		assertNotReadonly();
		this.resourceId = resourceId;
	}

	/**
	 * @return the current {@code State} of the a {@code Action}
	 */
	@Override
	public State getState() {
		return this.state;
	}

	/**
	 * @param state
	 * 		the target {@code State} of the a {@code Action}
	 */
	public void setState(State state) {
		assertNotReadonly();
		this.state = state;
	}

	/**
	 * @return the type of the {@code Resource} this {@code Action} acts on
	 */
	public String getResourceType() {
		return this.resourceType;
	}

	/**
	 * @param resourceType
	 * 		the resource type
	 */
	public void setResourceType(String resourceType) {
		assertNotReadonly();
		this.resourceType = resourceType;
	}

	/**
	 * Sets the resource type and id from the given {@link Resource}
	 *
	 * @param resource
	 * 		the resource from which to get the type and id
	 */
	public void setResource(Resource resource) {
		assertNotReadonly();
		this.resourceType = resource.getType();
		this.resourceId = resource.getId();
	}

	/**
	 * @deprecated use {@link #isResourceDefined()}
	 */
	@Deprecated
	public boolean hasResourceDefined() {
		return isNotEmpty(this.resourceType) && isNotEmpty(this.resourceId);
	}

	/**
	 * Returns true if the {@link #getResourceType()} and {@link #getResourceId()} have defined values
	 *
	 * @return true if the {@link #getResourceType()} and {@link #getResourceId()} have defined values
	 */
	public boolean isResourceDefined() {
		return isNotEmpty(this.resourceType) && isNotEmpty(this.resourceId);
	}

	/**
	 * Returns the {@link Locator} for the {@link Resource} for this action
	 *
	 * @return the {@link Locator} for the {@link Resource} for this action
	 *
	 * @throws IllegalStateException
	 * 		if the resource is not defined
	 */
	public Locator getResourceLocator() {
		if (!isResourceDefined())
			throw new IllegalStateException("Resource not set on " + getLocator());
		return Resource.locatorFor(this.resourceType, this.resourceId);
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
	 * @param change
	 *        {@code IValueChange} to be applied to the {@code Resource}
	 *
	 * @return <tt>true</tt> (as specified by {@link Collection#add})
	 */
	public boolean addChange(IValueChange<? extends IValue<?>> change) {
		assertNotReadonly();
		initChanges();
		return this.changes.add(change);
	}

	/**
	 * @return the list of {@code IValueChange} attached to the {@code Action} start
	 */
	public List<IValueChange<? extends IValue<?>>> getChanges() {
		if (this.changes == null)
			return Collections.emptyList();
		return this.changes;
	}

	/**
	 * @return the iterator of {@code IValueChange} attached to the {@code Action} start
	 */
	public Iterator<IValueChange<? extends IValue<?>>> changesIterator() {
		if (this.changes == null)
			return Collections.emptyIterator();
		return this.changes.iterator();
	}

	@Override
	public Activity getParent() {
		return this.parent;
	}

	@Override
	public Activity getRootElement() {
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
		clone.resourceId = this.resourceId;
		clone.resourceType = this.resourceType;
		clone.state = this.state;
		clone.locator = this.locator;

		if (this.changes != null) {
			for (IValueChange<? extends IValue<?>> change : getChanges()) {
				clone.addChange(change.getClone());
			}
		}

		if (this.policyDefs != null)
			clone.policyDefs = this.policyDefs.getClone();
		return clone;
	}

	@Override
	public void setReadOnly() {
		if (this.policyDefs != null)
			this.policyDefs.setReadOnly();
		if (this.changes != null) {
			for (IValueChange<? extends IValue<?>> change : changes) {
				change.setReadOnly();
			}
		}
		super.setReadOnly();
	}

	@Override
	public PolicyDefs getPolicyDefs() {
		if (this.policyDefs == null)
			throw new StrolchPolicyException(getLocator() + " has no Policies defined!");
		return this.policyDefs;
	}

	@Override
	public PolicyDef getPolicyDef(Class<?> clazz) {
		return getPolicyDefs().getPolicyDef(clazz);
	}

	@Override
	public PolicyDef getPolicyDef(String type) {
		return getPolicyDefs().getPolicyDef(type);
	}

	@Override
	public PolicyDef getPolicyDef(Class<?> clazz, PolicyDef defaultDef) {
		if (!hasPolicyDefs())
			return defaultDef;
		return getPolicyDefs().getPolicyDef(clazz, defaultDef);
	}

	@Override
	public PolicyDef getPolicyDef(String type, PolicyDef defaultDef) {
		if (!hasPolicyDefs())
			return defaultDef;
		return getPolicyDefs().getPolicyDef(type, defaultDef);
	}

	@Override
	public boolean hasPolicyDefs() {
		return this.policyDefs != null;
	}

	@Override
	public boolean hasPolicyDef(String type) {
		return this.policyDefs != null && this.policyDefs.hasPolicyDef(type);
	}

	@Override
	public boolean hasPolicyDef(Class<?> clazz) {
		return this.policyDefs != null && this.policyDefs.hasPolicyDef(clazz);
	}

	@Override
	public void setPolicyDefs(PolicyDefs policyDefs) {
		assertNotReadonly();
		this.policyDefs = policyDefs;
		this.policyDefs.setParent(this);
	}

	@Override
	public void addOrUpdate(PolicyDef policyDef) {
		assertNotReadonly();
		DBC.PRE.assertNotNull("policyDef", policyDef);
		if (this.policyDefs == null) {
			this.policyDefs = new PolicyDefs();
			this.policyDefs.setParent(this);
		}
		this.policyDefs.addOrUpdate(policyDef);
	}

	@Override
	public Locator getLocator() {
		if (this.locator == null) {
			LocatorBuilder lb = new LocatorBuilder();
			if (this.parent != null)
				this.parent.fillLocator(lb);
			fillLocator(lb);
			this.locator = lb.build();
		}
		return this.locator;
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
		assertNotReadonly();
		this.parent = activity;
	}

	@Override
	public Long getStart() {
		long start = Long.MAX_VALUE;
		if (this.changes == null)
			return start;
		for (IValueChange<?> change : this.changes) {
			start = Math.min(start, change.getTime());
		}
		return start;
	}

	@Override
	public Long getEnd() {
		long end = 0L;
		if (this.changes == null)
			return end;
		for (IValueChange<?> change : this.changes) {
			end = Math.max(end, change.getTime());
		}
		return end;
	}

	@Override
	public <U, T extends Parameter<U>> T findRelationParam(String paramKey) {
		return findParameter(BAG_RELATIONS, paramKey);
	}

	@Override
	public <U, T extends Parameter<U>> T findRelationParam(String paramKey, boolean assertExists) {
		return findParameter(BAG_RELATIONS, paramKey, assertExists);
	}

	@Override
	public <U, T extends Parameter<U>> T findParameter(String bagKey, String paramKey) {

		T parameter = getParameter(bagKey, paramKey);
		if (parameter != null)
			return parameter;

		return this.parent.findParameter(bagKey, paramKey);
	}

	@Override
	public <U, T extends Parameter<U>> T findParameter(String bagKey, String paramKey, boolean assertExists)
			throws StrolchModelException {

		T parameter = getParameter(bagKey, paramKey);
		if (parameter != null)
			return parameter;

		parameter = this.parent == null ? null : this.parent.findParameter(bagKey, paramKey);
		if (assertExists && parameter == null) {
			String msg = "The Parameter {0} does not exist";
			throw new StrolchModelException(MessageFormat.format(msg, getLocator().append(Tags.BAG, bagKey, paramKey)));
		}

		return parameter;
	}

	@Override
	public PolicyDef findPolicy(Class<?> clazz, PolicyDef defaultDef) throws StrolchModelException {
		return findPolicy(clazz.getSimpleName(), defaultDef);
	}

	@Override
	public PolicyDef findPolicy(String className, PolicyDef defaultDef) throws StrolchModelException {
		if (hasPolicyDef(className))
			return getPolicyDef(className);

		if (this.parent == null) {
			if (defaultDef != null)
				return defaultDef;

			String msg = "The PolicyDef {0} does not exist";
			throw new StrolchModelException(MessageFormat.format(msg, className));
		}

		return this.parent.findPolicy(className, defaultDef);
	}

	@Override
	public <T> T accept(StrolchElementVisitor<T> visitor) {
		return visitor.visitAction(this);
	}
}
