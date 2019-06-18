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
package li.strolch.policy;

import java.util.Set;

import li.strolch.model.policy.PolicyDef;
import li.strolch.model.policy.PolicyDefVisitor;
import li.strolch.persistence.api.StrolchTransaction;

/**
 * <p>
 * The {@link PolicyHandler} is Strolch's mechanism of dependency injection
 * </p>
 *
 * <p>
 * The resolving of a policy instance is handled by a {@link PolicyDefVisitor}
 * <p>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface PolicyHandler {

	/**
	 * The type of policies known by this {@link PolicyHandler}
	 *
	 * @return types of policies known by this {@link PolicyHandler}
	 */
	Set<String> getPolicyTypes();

	/**
	 * Returns the keys defined for the given policy type
	 *
	 * @param type
	 * 		the type of policy for which to return the keys
	 *
	 * @return the keys defined for the given policy type
	 */
	Set<String> getPolicyKeysByType(Class<?> type);

	/**
	 * Returns the keys defined for the given policy type
	 *
	 * @param type
	 * 		the type of policy for which to return the keys
	 *
	 * @return the keys defined for the given policy type
	 */
	Set<String> getPolicyKeysByType(String type);

	/**
	 * Instantiates the actual policy by resolving the {@link PolicyDef} using a {@link PolicyDefVisitor}
	 *
	 * @param policyDef
	 * 		the {@link PolicyDef} referencing a concrete policy
	 * @param tx
	 * 		the current transaction for which the policy is instantiated
	 *
	 * @return the instantiated instance of the referenced policy
	 */
	<T extends StrolchPolicy> T getPolicy(PolicyDef policyDef, StrolchTransaction tx);

	/**
	 * Returns true, if the policy definition is known
	 *
	 * @param policyDef
	 * 		the policy definition to check for
	 *
	 * @return true if the policy definition is known, false otherwise
	 */
	boolean isPolicyDefAvailable(PolicyDef policyDef);

	/**
	 * Reload the policies configuration
	 */
	void reloadPolicies();
}
