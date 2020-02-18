/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.model.policy;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import li.strolch.exception.StrolchPolicyException;
import li.strolch.model.Locator;
import li.strolch.model.StrolchElement;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.utils.dbc.DBC;

/**
 * The {@link PolicyDefs} contains the policy configuration of any {@link StrolchRootElement} which requires policies
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PolicyDefs {

	private StrolchElement parent;
	private Map<String, PolicyDef> policyDefMap;
	private boolean readOnly;

	public PolicyDefs() {
		this.policyDefMap = new HashMap<>(1, 1.0F);
	}

	public void setParent(StrolchElement parent) {
		assertNotReadonly();
		this.parent = parent;
	}

	public StrolchElement getParent() {
		return this.parent;
	}

	public Set<String> getPolicyTypes() {
		return this.policyDefMap.keySet();
	}

	public PolicyDef getPolicyDef(String type) {
		return getPolicyDef(type, null);
	}

	public PolicyDef getPolicyDef(String type, PolicyDef defaultDef) {
		if (!this.policyDefMap.containsKey(type)) {
			if (defaultDef != null)
				return defaultDef;

			throw new StrolchPolicyException(
					"The PolicyDef does not exist with type " + type + " on " + this.parent.getLocator());
		}
		return this.policyDefMap.get(type);
	}

	public boolean hasPolicyDef(String type) {
		return this.policyDefMap.containsKey(type);
	}

	public boolean hasPolicyDefs() {
		return !this.policyDefMap.isEmpty();
	}

	public void addOrUpdate(PolicyDef policyDef) {
		assertNotReadonly();
		DBC.PRE.assertNotNull("policyDef", policyDef);
		this.policyDefMap.put(policyDef.getType(), policyDef);
	}

	public void remove(String type) {
		assertNotReadonly();
		this.policyDefMap.remove(type);
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("PolicyDefs [parent=");
		sb.append(this.parent == null ? "null" : this.parent.getLocator());
		sb.append(", policyDefMap=");
		sb.append(this.policyDefMap);
		sb.append("]");
		return sb.toString();
	}

	public boolean isReadOnly() {
		return this.readOnly;
	}

	public void setReadOnly() {
		this.readOnly = true;
	}

	protected void assertNotReadonly() {
		if (this.readOnly) {
			throw new IllegalStateException(
					"The element " + getLocator() + " is currently readOnly, to modify clone first!");
		}
	}

	/**
	 * Returns a clone of this {@link PolicyDefs}
	 *
	 * @return a clone of this {@link PolicyDefs}
	 */
	public PolicyDefs getClone() {
		PolicyDefs clone = new PolicyDefs();
		for (PolicyDef policyDef : this.policyDefMap.values()) {
			clone.addOrUpdate(policyDef.getClone());
		}
		return clone;
	}

	public Locator getLocator() {
		if (this.parent == null)
			return Locator.valueOf(Tags.POLICIES);
		return this.parent.getLocator().append(Tags.POLICIES);
	}
}
