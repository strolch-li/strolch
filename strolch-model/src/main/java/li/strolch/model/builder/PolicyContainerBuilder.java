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

import li.strolch.model.ParameterBagContainer;
import li.strolch.model.PolicyContainer;

import static li.strolch.model.builder.BuilderHelper.buildParamName;

public class PolicyContainerBuilder<T extends ParameterBagContainerBuilder<T>> extends ParameterBagContainerBuilder<T> {

	private PoliciesBuilder<T> policies;

	public PolicyContainerBuilder(String id, String type) {
		super(id, buildParamName(id), type);
	}

	public PolicyContainerBuilder(String id, String name, String type) {
		super(id, name, type);
	}

	public PoliciesBuilder<T> policies() {
		if (this.policies == null) {
			@SuppressWarnings("unchecked") T t = (T) this;
			this.policies = new PoliciesBuilder<>(t);
		}
		return policies;
	}

	protected void applyPolicyContainer(PolicyContainer element) {
		applyParameters((ParameterBagContainer) element);

		if (this.policies != null)
			this.policies.build(element);
	}
}
