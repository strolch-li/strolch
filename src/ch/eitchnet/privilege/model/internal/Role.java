/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.model.internal;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import ch.eitchnet.privilege.i18n.PrivilegeException;
import ch.eitchnet.privilege.model.PrivilegeRep;
import ch.eitchnet.privilege.model.RoleRep;

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
 * @author rvonburg
 */
public final class Role {

	private final String name;
	private final Map<String, Privilege> privilegeMap;

	/**
	 * Default constructor
	 * 
	 * @param name
	 *            the name of the role
	 * @param privilegeMap
	 *            a map of {@link Privilege}s granted to this role
	 */
	public Role(String name, Map<String, Privilege> privilegeMap) {

		if (name == null || name.isEmpty()) {
			throw new PrivilegeException("No name defined!");
		}
		if (privilegeMap == null) {
			throw new PrivilegeException("No privileges defined!");
		}

		this.name = name;
		this.privilegeMap = Collections.unmodifiableMap(privilegeMap);
	}

	/**
	 * Construct {@link Role} from its representation {@link RoleRep}
	 * 
	 * @param roleRep
	 *            the representation from which to create the {@link Role}
	 */
	public Role(RoleRep roleRep) {

		String name = roleRep.getName();
		if (name == null || name.isEmpty()) {
			throw new PrivilegeException("No name defined!");
		}

		if (roleRep.getPrivilegeMap() == null) {
			throw new PrivilegeException("No privileges defined!");
		}

		// build privileges from reps
		Map<String, Privilege> privilegeMap = new HashMap<String, Privilege>(roleRep.getPrivilegeMap().size());
		for (String privilegeName : roleRep.getPrivilegeMap().keySet()) {
			privilegeMap.put(privilegeName, new Privilege(roleRep.getPrivilegeMap().get(privilegeName)));
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
	 * Returns the {@link Map} of {@link Privilege}s which are granted to this {@link Role}
	 * 
	 * @return the {@link Map} of {@link Privilege}s which are granted to this {@link Role}
	 */
	public Map<String, Privilege> getPrivilegeMap() {
		return this.privilegeMap;
	}

	/**
	 * Determines if this {@link Role} has the {@link Privilege} with the given name
	 * 
	 * @param name
	 *            the name of the {@link Privilege}
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
		Map<String, PrivilegeRep> privilegeMap = new HashMap<String, PrivilegeRep>();
		for (String privilegeName : this.privilegeMap.keySet()) {
			privilegeMap.put(privilegeName, this.privilegeMap.get(privilegeName).asPrivilegeRep());
		}
		return new RoleRep(this.name, privilegeMap);
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("Role [name=");
		builder.append(this.name);
		builder.append(", privileges=");
		builder.append(this.privilegeMap.keySet());
		builder.append("]");
		return builder.toString();
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
		return result;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
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
			if (other.name != null)
				return false;
		} else if (!this.name.equals(other.name))
			return false;
		return true;
	}
}
