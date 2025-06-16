/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.exception.StrolchPolicyException;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.policy.PolicyDefs;

/**
 * A {@link PolicyContainer} has a reference to {@link PolicyDefs} on which Policy configurations are stored for the
 * given object
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface PolicyContainer {

	/**
	 * Returns the reference to the {@link PolicyDefs}
	 *
	 * @return the reference to the {@link PolicyDefs}
	 *
	 * @throws StrolchPolicyException if no {@link PolicyDefs} are available
	 */
	PolicyDefs getPolicyDefs() throws StrolchPolicyException;

	/**
	 * @return true if this container has {@link PolicyDefs}, false if not
	 */
	boolean hasPolicyDefs();

	/**
	 * Returns true if this container has the {@link PolicyDef} with the given type, false if not
	 *
	 * @param type the type of policy def to return
	 *
	 * @return true if this container has the {@link PolicyDef} with the given type, false if not
	 */
	boolean hasPolicyDef(String type);

	/**
	 * Returns true if this container has the {@link PolicyDef} with the given type, false if not
	 *
	 * @param clazz the type of policy def to return
	 *
	 * @return true if this container has the {@link PolicyDef} with the given type, false if not
	 */
	boolean hasPolicyDef(Class<?> clazz);

	/**
	 * Returns the {@link PolicyDef} for the given type
	 *
	 * @param type the type of policy def to return
	 *
	 * @return the policy def of the given type
	 */
	PolicyDef getPolicyDef(String type);

	/**
	 * Returns the {@link PolicyDef} for the given type
	 *
	 * @param type       the type of policy def to return
	 * @param defaultDef the default policy definition to return if the given type is not defined
	 *
	 * @return the policy def of the given type
	 */
	PolicyDef getPolicyDef(String type, PolicyDef defaultDef);

	/**
	 * Returns the {@link PolicyDef} for the given class
	 *
	 * @param clazz the type of policy def to return
	 *
	 * @return the policy def of the given class
	 */
	PolicyDef getPolicyDef(Class<?> clazz);

	/**
	 * Returns the {@link PolicyDef} for the given class
	 *
	 * @param clazz      the type of policy def to return
	 * @param defaultDef the default policy definition to return if the given type is not defined
	 *
	 * @return the policy def of the given class
	 */
	PolicyDef getPolicyDef(Class<?> clazz, PolicyDef defaultDef);

	/**
	 * Set the reference to the {@link PolicyDefs}
	 *
	 * @param policyDefs the {@link PolicyDefs} to set
	 */
	void setPolicyDefs(PolicyDefs policyDefs);

	/**
	 * Add or update the given {@link PolicyDef} to this container
	 *
	 * @param policyDef the {@link PolicyDef} to add or update
	 */
	void addOrUpdate(PolicyDef policyDef);
}
