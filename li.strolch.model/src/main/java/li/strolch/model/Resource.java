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
import java.util.*;

import li.strolch.exception.StrolchException;
import li.strolch.exception.StrolchModelException;
import li.strolch.exception.StrolchPolicyException;
import li.strolch.model.Locator.LocatorBuilder;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.policy.PolicyDefs;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.visitor.StrolchElementVisitor;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class Resource extends AbstractStrolchRootElement implements StrolchRootElement, Comparable<Resource> {

	protected Locator locator;
	protected Version version;
	protected Map<String, StrolchTimedState<IValue<?>>> timedStateMap;
	protected PolicyDefs policyDefs;

	/**
	 * Empty constructor - for marshalling only!
	 */
	public Resource() {
		super();
	}

	/**
	 * Default constructor
	 *
	 * @param id
	 * 		the id
	 * @param name
	 * 		the name
	 * @param type
	 * 		the type
	 */
	public Resource(String id, String name, String type) {
		super(id, name, type);
	}

	@Override
	public String getObjectType() {
		return Tags.RESOURCE;
	}

	@Override
	public boolean hasVersion() {
		return this.version != null;
	}

	@Override
	public Version getVersion() {
		return this.version;
	}

	@Override
	public void setVersion(Version version) throws IllegalArgumentException, IllegalStateException {
		if (version != null && !getLocator().equals(version.getLocator())) {
			String msg = "Illegal version as locator is not same: Element: {0} Version: {1}";
			throw new IllegalArgumentException(MessageFormat.format(msg, getLocator(), version));
		}

		this.version = version;
	}

	@SuppressWarnings("unchecked")
	public void addTimedState(StrolchTimedState<?> strolchTimedState) {
		assertNotReadonly();
		if (this.timedStateMap == null) {
			this.timedStateMap = new HashMap<>(1, 1.0F);
		}

		if (this.timedStateMap.containsKey(strolchTimedState.getId())) {
			String msg = "A TimedState already exists with id {0} on {1}";
			throw new StrolchException(MessageFormat.format(msg, strolchTimedState.getId(), getLocator()));
		}
		this.timedStateMap.put(strolchTimedState.getId(), (StrolchTimedState<IValue<?>>) strolchTimedState);
		strolchTimedState.setParent(this);
	}

	public <T extends StrolchTimedState<? extends IValue<?>>> T getTimedState(String id) {
		return getTimedState(id, false);
	}

	public <T extends StrolchTimedState<? extends IValue<?>>> T getTimedState(String id, boolean assertExists) {
		if (this.timedStateMap == null) {
			if (assertExists) {
				String msg = "The TimedState {0} does not exist";
				throw new StrolchModelException(MessageFormat.format(msg, getLocator().append(Tags.STATE, id)));
			}
			return null;
		}

		@SuppressWarnings("unchecked")
		T timedState = (T) this.timedStateMap.get(id);
		if (timedState == null && assertExists) {
			String msg = "The TimedState {0} does not exist";
			throw new StrolchModelException(MessageFormat.format(msg, getLocator().append(Tags.STATE, id)));
		}

		return timedState;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public <T extends StrolchTimedState> T removeTimedState(String id) {
		assertNotReadonly();
		if (this.timedStateMap == null) {
			return null;
		}
		return (T) this.timedStateMap.remove(id);
	}

	public Set<String> getTimedStateKeySet() {
		if (this.timedStateMap == null) {
			return Collections.emptySet();
		}
		return new HashSet<>(this.timedStateMap.keySet());
	}

	public List<StrolchTimedState<IValue<?>>> getTimedStates() {
		if (this.timedStateMap == null) {
			return Collections.emptyList();
		}
		return new ArrayList<>(this.timedStateMap.values());
	}

	public boolean hasTimedStates() {
		return this.timedStateMap != null && !this.timedStateMap.isEmpty();
	}

	public boolean hasTimedState(String id) {
		return this.timedStateMap != null && this.timedStateMap.containsKey(id);
	}

	@Override
	public PolicyDefs getPolicyDefs() {
		if (this.policyDefs == null)
			throw new StrolchPolicyException(getLocator() + " has no Policies defined!");
		return this.policyDefs;
	}

	@Override
	public PolicyDef getPolicyDef(Class<?> clazz) {
		return getPolicyDefs().getPolicyDef(clazz.getSimpleName());
	}

	@Override
	public PolicyDef getPolicyDef(String type) {
		return getPolicyDefs().getPolicyDef(type);
	}

	@Override
	public boolean hasPolicyDefs() {
		return this.policyDefs != null;
	}

	@Override
	public boolean hasPolicyDef(String type) {
		return this.policyDefs != null && policyDefs.hasPolicyDef(type);
	}

	@Override
	public void setPolicyDefs(PolicyDefs policyDefs) {
		assertNotReadonly();
		this.policyDefs = policyDefs;
		this.policyDefs.setParent(this);
	}

	@Override
	public Resource getClone() {
		return getClone(false);
	}

	@Override
	public Resource getClone(boolean withVersion) {

		Resource clone = new Resource();

		super.fillClone(clone);

		if (this.timedStateMap != null) {
			for (StrolchTimedState<IValue<?>> timedState : this.timedStateMap.values()) {
				clone.addTimedState(timedState.getClone());
			}
		}

		if (this.policyDefs != null)
			clone.setPolicyDefs(this.policyDefs.getClone());

		if (withVersion)
			clone.setVersion(this.version);

		return clone;
	}

	@Override
	public void setReadOnly() {
		if (this.policyDefs != null)
			this.policyDefs.setReadOnly();
		if (this.timedStateMap != null) {
			for (StrolchTimedState<IValue<?>> timedState : this.timedStateMap.values()) {
				timedState.setReadOnly();
			}
		}
		super.setReadOnly();
	}

	@Override
	public void fillLocator(LocatorBuilder lb) {
		lb.append(Tags.RESOURCE).append(getType()).append(getId());
	}

	@Override
	public Locator getLocator() {
		if (this.locator == null) {
			LocatorBuilder lb = new LocatorBuilder();
			fillLocator(lb);
			this.locator = lb.build();
		}
		return this.locator;
	}

	@Override
	public StrolchElement getParent() {
		return null;
	}

	@Override
	public Resource getRootElement() {
		return this;
	}

	@Override
	public boolean isRootElement() {
		return true;
	}

	@Override
	public <T> T accept(StrolchElementVisitor<T> visitor) {
		return visitor.visitResource(this);
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {

		StringBuilder builder = new StringBuilder();

		builder.append("Resource [id=");
		builder.append(this.id);
		builder.append(", name=");
		builder.append(this.name);
		builder.append(", type=");
		builder.append(this.type);
		builder.append(", version=");
		builder.append(this.version);

		return builder.toString();
	}

	@Override
	public int compareTo(Resource o) {
		return getId().compareTo(o.getId());
	}

	public static Locator locatorFor(String type, String id) {
		return Locator.valueOf(Tags.RESOURCE, type, id);
	}
}
