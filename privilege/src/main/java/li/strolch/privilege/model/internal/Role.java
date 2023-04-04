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
package li.strolch.privilege.model.internal;

import java.util.*;
import java.util.Map.Entry;

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.IPrivilege;
import li.strolch.privilege.model.PrivilegeRep;
import li.strolch.privilege.model.RoleRep;
import li.strolch.utils.helper.StringHelper;

/**
 * <p>
 * A {@link User} is assigned a set of roles. These roles have a set of privileges assigned to them by name and they
 * define the privileges granted to a user with this role
 * </p>
 *
 * <p>
 * Note: This is an internal object which is not to be serialized or passed to clients, {@link RoleRep}s are used for
 * that
 * </p>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public final class Role {

	private final String name;
	private final Map<String, IPrivilege> privilegeMap;

	/**
	 * Default constructor
	 *
	 * @param name
	 * 		the name of the role
	 * @param privilegeMap
	 * 		a map of {@link IPrivilege}s granted to this role
	 */
	public Role(String name, Map<String, IPrivilege> privilegeMap) {

		if (StringHelper.isEmpty(name)) {
			throw new PrivilegeException("No name defined!"); //$NON-NLS-1$
		}
		if (privilegeMap == null) {
			throw new PrivilegeException("No privileges defined!"); //$NON-NLS-1$
		}

		this.name = name;
		this.privilegeMap = Collections.unmodifiableMap(privilegeMap);
	}

	/**
	 * Construct {@link Role} from its representation {@link RoleRep}
	 *
	 * @param roleRep
	 * 		the representation from which to create the {@link Role}
	 */
	public Role(RoleRep roleRep) {

		String name = roleRep.getName();
		if (StringHelper.isEmpty(name)) {
			throw new PrivilegeException("No name defined!"); //$NON-NLS-1$
		}

		if (roleRep.getPrivileges() == null) {
			throw new PrivilegeException("Privileges may not be null!"); //$NON-NLS-1$
		}

		// build privileges from rep
		Map<String, IPrivilege> privilegeMap = new HashMap<>(roleRep.getPrivileges().size());
		for (PrivilegeRep privilege : roleRep.getPrivileges()) {
			privilegeMap.put(privilege.getName(), new PrivilegeImpl(privilege));
		}

		this.name = name;
		this.privilegeMap = Collections.unmodifiableMap(privilegeMap);
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * Returns the {@link Set} of names for the currently stored {@link IPrivilege Privileges}
	 *
	 * @return the {@link Set} of names for the currently stored {@link IPrivilege Privileges}
	 */
	public Set<String> getPrivilegeNames() {
		return this.privilegeMap.keySet();
	}

	/**
	 * Returns the {@link IPrivilege} for the given name, null if it does not exist
	 *
	 * @return the {@link IPrivilege} for the given name, null if it does not exist
	 */
	public IPrivilege getPrivilege(String name) {
		return this.privilegeMap.get(name);
	}

	/**
	 * Determines if this {@link Role} has the {@link IPrivilege} with the given name
	 *
	 * @param name
	 * 		the name of the {@link IPrivilege}
	 *
	 * @return true if this {@link Role} has the {@link IPrivilege} with the given name
	 */
	public boolean hasPrivilege(String name) {
		return this.privilegeMap.containsKey(name);
	}

	/**
	 * @return a {@link RoleRep} which is a representation of this object used to serialize and view on clients
	 */
	public RoleRep asRoleRep() {
		List<PrivilegeRep> privileges = new ArrayList<>();
		for (Entry<String, IPrivilege> entry : this.privilegeMap.entrySet()) {
			privileges.add(entry.getValue().asPrivilegeRep());
		}
		return new RoleRep(this.name, privileges);
	}

	/**
	 * Returns a string representation of this object displaying its concrete type and its values
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		String builder = "Role [name=" + this.name + ", privileges=" + this.privilegeMap.keySet() + "]";
		return builder;
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
		Role other = (Role) obj;
		if (this.name == null) {
			return other.name == null;
		} else
			return this.name.equals(other.name);
	}
}
