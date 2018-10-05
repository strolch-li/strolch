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

import java.util.Set;

import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.policy.PrivilegePolicy;

/**
 * <p>
 * {@link IPrivilege} is the main model object for Privilege. A {@link Role} has a set of Privileges assigned to it
 * which defines the privileges a logged in user with that role has. If the {@link IPrivilege} has a {@link
 * PrivilegePolicy} defined, then that policy will be used for finer granularity and with the deny and allow lists
 * configured which is used to evaluate if privilege is granted to a {@link Restrictable}
 * </p>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface IPrivilege {

	/**
	 * @return a {@link PrivilegeRep} which is a representation of this object used to serialize and view on clients
	 */
	public abstract PrivilegeRep asPrivilegeRep();

	/**
	 * @return the name
	 */
	public abstract String getName();

	/**
	 * @return the policy
	 */
	public abstract String getPolicy();

	/**
	 * @return the allAllowed
	 */
	public abstract boolean isAllAllowed();

	/**
	 * @return the allowList
	 */
	public abstract Set<String> getAllowList();

	/**
	 * @return the denyList
	 */
	public abstract Set<String> getDenyList();

	/**
	 * @return true if there are values in the allow list
	 */
	public abstract boolean hasAllowed();

	/**
	 * @return if the value is in the allow list
	 */
	public abstract boolean isAllowed(String value);

	/**
	 * @return true if there are values in the deny list
	 */
	public abstract boolean hasDenied();

	/**
	 * @return true if the value is in the deny list
	 */
	public abstract boolean isDenied(String value);

}