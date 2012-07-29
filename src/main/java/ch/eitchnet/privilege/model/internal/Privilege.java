/*
 * Copyright (c) 2010 - 2012
 * 
 * This file is part of Privilege.
 *
 * Privilege is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Privilege is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Privilege.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.privilege.model.internal;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.i18n.PrivilegeException;
import ch.eitchnet.privilege.model.PrivilegeRep;
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.policy.PrivilegePolicy;

/**
 * <p>
 * {@link Privilege} is the main model object for Privilege. A {@link Role} has a set of Privileges assigned to it which
 * defines the privileges a logged in user with that role has. If the {@link Privilege} has a {@link PrivilegePolicy}
 * defined, then that policy will be used for finer granularity and with the deny and allow lists configured which is
 * used to evaluate if privilege is granted to a {@link Restrictable}
 * </p>
 * 
 * <p>
 * {@link Privilege}s have allow and deny rules which the configured {@link PrivilegeHandler} uses to
 * </p>
 * 
 * <p>
 * Note: This is an internal object which is not to be serialized or passed to clients, {@link PrivilegeRep}s are used
 * for that
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public final class Privilege {

	private final String name;
	private final String policy;
	private final boolean allAllowed;
	private final Set<String> denyList;
	private final Set<String> allowList;

	/**
	 * Default constructor
	 * 
	 * @param name
	 *            the name of this privilege, which is unique to all privileges known in the {@link PrivilegeHandler}
	 * @param policy
	 *            the {@link PrivilegePolicy} configured to evaluate if the privilege is granted. If null, then
	 *            privilege is granted
	 * @param allAllowed
	 *            a boolean defining if a {@link Role} with this {@link Privilege} has unrestricted access to a
	 *            {@link Restrictable} in which case the deny and allow lists are ignored and can be null
	 * @param denyList
	 *            a list of deny rules for this {@link Privilege}, can be null if all allowed
	 * @param allowList
	 *            a list of allow rules for this {@link Privilege}, can be null if all allowed
	 */
	public Privilege(String name, String policy, boolean allAllowed, Set<String> denyList, Set<String> allowList) {

		if (name == null || name.isEmpty()) {
			throw new PrivilegeException("No name defined!");
		}
		this.name = name;

		// if not all allowed, then validate that deny and allow lists are defined
		if (allAllowed) {

			this.allAllowed = true;

			// all allowed means no policy will be used
			this.policy = null;

			this.denyList = Collections.emptySet();
			this.allowList = Collections.emptySet();
		} else {

			this.allAllowed = false;

			// not all allowed, so policy must be set
			if (policy == null || policy.isEmpty()) {
				throw new PrivilegeException("All is not allowed and no Policy defined!");
			}
			this.policy = policy;

			if (denyList == null) {
				throw new PrivilegeException("All is not allowed and no denyList defined!");
			}
			this.denyList = Collections.unmodifiableSet(denyList);

			if (allowList == null) {
				throw new PrivilegeException("All is not allowed and no allowList defined!");
			}
			this.allowList = Collections.unmodifiableSet(allowList);
		}
	}

	/**
	 * Constructs a {@link Privilege} from the {@link PrivilegeRep}
	 * 
	 * @param privilegeRep
	 *            the {@link PrivilegeRep} from which to create the {@link Privilege}
	 */
	public Privilege(PrivilegeRep privilegeRep) {
		this(privilegeRep.getName(), privilegeRep.getPolicy(), privilegeRep.isAllAllowed(), privilegeRep.getDenyList(),
				privilegeRep.getDenyList());
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * @return the policy
	 */
	public String getPolicy() {
		return this.policy;
	}

	/**
	 * @return the allAllowed
	 */
	public boolean isAllAllowed() {
		return this.allAllowed;
	}

	/**
	 * @return the allowList
	 */
	public Set<String> getAllowList() {
		return this.allowList;
	}

	/**
	 * @return the denyList
	 */
	public Set<String> getDenyList() {
		return this.denyList;
	}

	/**
	 * @return a {@link PrivilegeRep} which is a representation of this object used to serialize and view on clients
	 */
	public PrivilegeRep asPrivilegeRep() {
		return new PrivilegeRep(this.name, this.policy, this.allAllowed, new HashSet<String>(this.denyList),
				new HashSet<String>(this.allowList));
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("Privilege [name=");
		builder.append(this.name);
		builder.append(", policy=");
		builder.append(this.policy);
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
		Privilege other = (Privilege) obj;
		if (this.name == null) {
			if (other.name != null)
				return false;
		} else if (!this.name.equals(other.name))
			return false;
		return true;
	}

}
