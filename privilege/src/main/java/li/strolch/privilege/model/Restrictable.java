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
package li.strolch.privilege.model;

import li.strolch.privilege.policy.PrivilegePolicy;

/**
 * <p>
 * Objects implementing this interface are used to grant/restrict privileges to them. A {@link PrivilegePolicy}
 * implements the logic on granting/restricting privileges for a {@link Restrictable} and the {@link
 * #getPrivilegeName()} is used to find the {@link IPrivilege} which has the associated {@link PrivilegePolicy} for
 * evaluating access
 * </p>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface Restrictable {

	/**
	 * Returns the name of the {@link IPrivilege} which is to be used to validate privileges against
	 *
	 * @return the name of the {@link IPrivilege} which is to be used to validate privileges against
	 */
	String getPrivilegeName();

	/**
	 * Returns the value which defines or describes what privilege is to be granted
	 *
	 * @return the value which defines or describes what privilege is to be granted
	 */
	Object getPrivilegeValue();
}
