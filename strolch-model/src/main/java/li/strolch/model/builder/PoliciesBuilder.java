/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.model.builder;

import li.strolch.model.PolicyContainer;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.policy.PolicyDefs;

public class PoliciesBuilder<T extends ParameterBagContainerBuilder<T>> {

	private final T builder;
	private final PolicyDefs policyDefs;

	public PoliciesBuilder(T builder) {
		this.builder = builder;
		this.policyDefs = new PolicyDefs();
	}

	public PoliciesBuilder<T> planning(String value) {
		return policy("PlanningPolicy", value);
	}

	public PoliciesBuilder<T> execution(String value) {
		return policy("ExecutionPolicy", value);
	}

	public PoliciesBuilder<T> confirmation(String value) {
		return policy("ConfirmationPolicy", value);
	}

	public PoliciesBuilder<T> activityArchival(String value) {
		return policy("ActivityArchivalPolicy", value);
	}

	public PoliciesBuilder<T> policy(String type, String value) {
		this.policyDefs.addOrUpdate(PolicyDef.valueOf(type, value));
		return this;
	}

	public T endPolicies() {
		return builder;
	}

	public void build(PolicyContainer element) {
		element.setPolicyDefs(this.policyDefs.getClone());
	}
}
