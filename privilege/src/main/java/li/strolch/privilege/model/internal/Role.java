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

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Privilege;
import li.strolch.privilege.model.PrivilegeRep;
import li.strolch.privilege.model.RoleRep;
import li.strolch.utils.dbc.DBC;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static li.strolch.utils.helper.StringHelper.isEmpty;

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
public record Role(String name, Map<String, Privilege> privilegeMap) {

	public Role(String name, Map<String, Privilege> privilegeMap) {
		DBC.PRE.assertNotEmpty("name must not be empty", name);
		DBC.PRE.assertNotNull("privilegeMap must not be null", privilegeMap);
		this.name = name;
		this.privilegeMap = Map.copyOf(privilegeMap);
	}

	/**
	 * Construct {@link Role} from its representation {@link RoleRep}
	 *
	 * @param roleRep the representation from which to create the {@link Role}
	 */
	public static Role of(RoleRep roleRep) {
		String name = roleRep.getName();
		if (isEmpty(name))
			throw new PrivilegeException("No name defined!");
		if (roleRep.getPrivileges() == null)
			throw new PrivilegeException("Privileges may not be null!");

		// build privileges from rep
		Map<String, Privilege> privilegeMap = new HashMap<>(roleRep.getPrivileges().size());
		roleRep.getPrivileges().values().forEach(p -> privilegeMap.put(p.getName(), Privilege.of(p)));

		return new Role(name, privilegeMap);
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * Returns the {@link Set} of names for the currently stored {@link Privilege Privileges}
	 *
	 * @return the {@link Set} of names for the currently stored {@link Privilege Privileges}
	 */
	public Set<String> getPrivilegeNames() {
		return this.privilegeMap.keySet();
	}

	/**
	 * Returns the {@link Privilege} for the given name, null if it does not exist
	 *
	 * @return the {@link Privilege} for the given name, null if it does not exist
	 */
	public Privilege getPrivilege(String name) {
		return this.privilegeMap.get(name);
	}

	/**
	 * Determines if this {@link Role} has the {@link Privilege} with the given name
	 *
	 * @param name the name of the {@link Privilege}
	 *
	 * @return true if this {@link Role} has the {@link Privilege} with the given name
	 */
	public boolean hasPrivilege(String name) {
		return this.privilegeMap.containsKey(name);
	}

	/**
	 * @return a {@link RoleRep} which is a representation of this object used to serialize and view on clients
	 */
	public RoleRep asRoleRep() {
		Map<String, PrivilegeRep> privileges = new HashMap<>();
		this.privilegeMap.values().forEach(p -> privileges.put(p.getName(), p.asPrivilegeRep()));
		return new RoleRep(this.name, privileges);
	}

	/**
	 * Returns a string representation of this object displaying its concrete type and its values
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "Role [name=" + this.name + ", privileges=" + this.privilegeMap.keySet() + "]";
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
