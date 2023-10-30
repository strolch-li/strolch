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

import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.policy.PrivilegePolicy;
import li.strolch.utils.dbc.DBC;

import java.util.HashSet;
import java.util.Set;

/**
 * <p>
 * {@link Privilege} is the main model object for Privilege. A {@link Role} has a set of Privileges assigned to it which
 * defines the privileges a logged in user with that role has. If the {@link Privilege} has a {@link PrivilegePolicy}
 * defined, then that policy will be used for finer granularity and with the deny and allow lists configured which is
 * used to evaluate if privilege is granted to a {@link Restrictable}
 * </p>
 *
 * @param name       the name of this privilege, which is unique to all privileges known in the
 *                   {@link PrivilegeHandler}
 * @param policy     the {@link PrivilegePolicy} configured to evaluate if the privilege is granted. If null, then
 *                   privilege is granted
 * @param allAllowed a boolean defining if a {@link Role} with this {@link Privilege} has unrestricted access to a
 *                   {@link Restrictable} in which case the deny and allow lists are ignored and can be null
 * @param denyList   a list of deny rules for this {@link Privilege}, can be null if all allowed
 * @param allowList  a list of allow rules for this {@link Privilege}, can be null if all allowed
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public record Privilege(String name, String policy, boolean allAllowed, Set<String> denyList, Set<String> allowList) {

	public Privilege(String name, String policy, boolean allAllowed, Set<String> denyList, Set<String> allowList) {
		DBC.PRE.assertNotEmpty("name must not be empty", name);
		DBC.PRE.assertNotEmpty("policy must not be empty", policy);
		DBC.PRE.assertNotNull("denyList must not be null", denyList);
		DBC.PRE.assertNotNull("allowList must not be null", allowList);

		this.name = name;
		this.allAllowed = allAllowed;
		this.policy = policy;
		this.denyList = Set.copyOf(denyList);
		this.allowList = Set.copyOf(allowList);
	}

	/**
	 * @return a {@link PrivilegeRep} which is a representation of this object used to serialize and view on clients
	 */
	public PrivilegeRep asPrivilegeRep() {
		return new PrivilegeRep(this.name, this.policy, this.allAllowed, new HashSet<>(this.denyList),
				new HashSet<>(this.allowList));
	}

	public String getName() {
		return this.name;
	}

	public String getPolicy() {
		return this.policy;
	}

	public boolean isAllAllowed() {
		return this.allAllowed;
	}

	public Set<String> getAllowList() {
		return this.allowList;
	}

	public Set<String> getDenyList() {
		return this.denyList;
	}

	public boolean hasAllowed() {
		return !this.allowList.isEmpty();
	}

	public boolean isAllowed(String value) {
		return this.allowList.contains(value);
	}

	public boolean hasDenied() {
		return !this.allowList.isEmpty();
	}

	public boolean isDenied(String value) {
		return this.denyList.contains(value);
	}

	@Override
	public String toString() {
		return "Privilege [name=" + this.name + ", policy=" + this.policy + "]";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Privilege other = (Privilege) obj;
		if (this.name == null)
			return other.name == null;
		return this.name.equals(other.name);
	}

	/**
	 * Constructs a {@link Privilege} from the {@link PrivilegeRep}
	 *
	 * @param privilegeRep the {@link PrivilegeRep} from which to create the {@link Privilege}
	 */
	public static Privilege of(PrivilegeRep privilegeRep) {
		return new Privilege(privilegeRep.getName(), privilegeRep.getPolicy(), privilegeRep.isAllAllowed(),
				privilegeRep.getDenyList(), privilegeRep.getAllowList());
	}
}