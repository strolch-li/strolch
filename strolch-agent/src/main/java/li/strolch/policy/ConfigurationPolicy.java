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
import li.strolch.model.policy.PolicyDef;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicyFileParser.PolicyModel;

import static li.strolch.model.StrolchModelConstants.PolicyConstants.POLICY_DEFAULT;
import static li.strolch.model.policy.PolicyDef.getJavaPolicy;
import static li.strolch.model.policy.PolicyDef.getKeyPolicy;

/**
 * The {@link ConfigurationPolicy} is used to access and update the Strolch configuration and policies at runtime.
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class ConfigurationPolicy extends StrolchPolicy {

	public ConfigurationPolicy(StrolchTransaction tx) {
		super(tx);
	}

	/**
	 * Returns the configuration {@link Resource}
	 *
	 * @return the configuration {@link Resource}
	 */
	public abstract Resource getConfigurationResource();

	/**
	 * Updates the configuration {@link Resource}
	 *
	 * @param resource the {@link Resource} to update
	 */
	public abstract void updateConfigurationResource(Resource resource);

	/**
	 * Returns the current {@link PolicyModel}
	 *
	 * @return the current {@link PolicyModel}
	 */
	public abstract PolicyModel getPolicyModel();

	/**
	 * Updates the {@link PolicyModel}
	 *
	 * @param policyModel the {@link PolicyModel} to update
	 */
	public abstract void updatePolicyModel(PolicyModel policyModel);

	public static ConfigurationPolicy getDefaultPolicy(StrolchTransaction tx) {
		PolicyDef defaultDef = getKeyPolicy(ConfigurationPolicy.class, POLICY_DEFAULT);
		PolicyDef fallbackDef = getJavaPolicy(ConfigurationPolicy.class, DefaultConfigurationPolicy.class);
		return tx.getPolicy(ConfigurationPolicy.class, defaultDef, fallbackDef);
	}
}
