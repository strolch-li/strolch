/*
 * Copyright (c) 2015-2025 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.policy;

import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicyFileParser.PolicyModel;

/**
 * The {@link DefaultConfigurationPolicy} is the default implementation of the {@link ConfigurationPolicy}.
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultConfigurationPolicy extends ConfigurationPolicy {

	/**
	 * Instantiate a new {@link DefaultConfigurationPolicy}
	 *
	 * @param tx the transaction for this policy
	 */
	public DefaultConfigurationPolicy(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public Resource getConfigurationResource() {
		return tx().getConfiguration();
	}

	@Override
	public void updateConfigurationResource(Resource resource) {
		tx().addOrUpdate(resource);
	}

	@Override
	public PolicyModel getPolicyModel() {
		PolicyHandler policyHandler = getComponent(PolicyHandler.class);
		return policyHandler.getPolicyModel();
	}

	@Override
	public void updatePolicyModel(PolicyModel policyModel) {
		PolicyHandler policyHandler = getComponent(PolicyHandler.class);
		policyHandler.updatePolicies(policyModel);
		policyHandler.savePolicies();
	}
}
