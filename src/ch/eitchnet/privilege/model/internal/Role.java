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
import java.util.HashSet;
import java.util.Set;

import ch.eitchnet.privilege.model.RoleRep;

/**
 * @author rvonburg
 * 
 */
public final class Role {

	private final String name;
	private final Set<String> privileges;

	/**
	 * 
	 * @param name
	 * @param privileges
	 */
	public Role(String name, Set<String> privileges) {
		this.name = name;
		this.privileges = Collections.unmodifiableSet(privileges);
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * Returns the {@link Set} of {@link Privilege} names which is granted to this {@link Role}
	 * 
	 * @return the {@link Set} of {@link Privilege} names which is granted to this
	 */
	public Set<String> getPrivileges() {
		return this.privileges;
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
		return this.privileges.contains(name);
	}

	/**
	 * @return a {@link RoleRep} which is a representation of this object used to serialize and view on clients
	 */
	public RoleRep asRoleRep() {
		return new RoleRep(this.name, new HashSet<String>(this.privileges));
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
		builder.append(this.privileges);
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
		result = prime * result + ((this.privileges == null) ? 0 : this.privileges.hashCode());
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
		if (this.privileges == null) {
			if (other.privileges != null)
				return false;
		} else if (!this.privileges.equals(other.privileges))
			return false;
		return true;
	}

}
