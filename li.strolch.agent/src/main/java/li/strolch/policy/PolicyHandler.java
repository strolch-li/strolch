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

import li.strolch.model.policy.PolicyDef;
import li.strolch.model.policy.PolicyDefVisitor;

/**
 * <p>
 * The {@link PolicyHandler} is Strolch's mechanism of dependency injection
 * </p>
 * 
 * <p>
 * Objects which require delegation can use a {@link PolicyConfiguration} element and then retrieve a Policy instance
 * from this {@link PolicyHandler}.
 * </p>
 * 
 * <p>
 * {@link PolicyConfiguration} have a mapping of a policy type, i.e. an interface for a specific delegation. This
 * interface has concrete implementations which are then returned by the {@link PolicyHandler} depending on the current
 * configuration
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
	 * Instantiates the actual policy by resolving the {@link PolicyDef} using a {@link PolicyDefVisitor}
	 * 
	 * @param policyDef
	 *            the {@link PolicyDef} referencing a concrete policy
	 * 
	 * @return the instantiated instance of the referenced policy
	 */
	public <T> T getPolicy(PolicyDef policyDef);
}
