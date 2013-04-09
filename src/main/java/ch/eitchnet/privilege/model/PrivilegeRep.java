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
package ch.eitchnet.privilege.model;

import java.io.Serializable;
import java.util.Set;

import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.policy.PrivilegePolicy;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * To keep certain details of the {@link IPrivilege} itself hidden from remote clients and make sure instances are only
 * edited by users with the correct privilege, this representational version is allowed to be viewed by remote clients
 * and simply wraps all public data from the {@link IPrivilege}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeRep implements Serializable {

	private static final long serialVersionUID = 1L;

	private String name;
	private String policy;
	private boolean allAllowed;
	private Set<String> denyList;
	private Set<String> allowList;

	/**
	 * Default constructor
	 * 
	 * @param name
	 *            the name of this privilege, which is unique to all privileges known in the {@link PrivilegeHandler}
	 * @param policy
	 *            the {@link PrivilegePolicy} configured to evaluate if the privilege is granted
	 * @param allAllowed
	 *            a boolean defining if a {@link Role} with this {@link IPrivilege} has unrestricted access to a
	 *            {@link Restrictable}
	 * @param denyList
	 *            a list of deny rules for this {@link IPrivilege}
	 * @param allowList
	 *            a list of allow rules for this {@link IPrivilege}
	 */
	public PrivilegeRep(String name, String policy, boolean allAllowed, Set<String> denyList, Set<String> allowList) {
		this.name = name;
		this.policy = policy;
		this.allAllowed = allAllowed;
		this.denyList = denyList;
		this.allowList = allowList;

		validate();
	}

	/**
	 * Validates that all required fields are set
	 */
	public void validate() {

		if (StringHelper.isEmpty(this.name)) {
			throw new PrivilegeException("No name defined!");
		}

		if (StringHelper.isEmpty(this.policy)) {
			throw new PrivilegeException("policy is null!");
		}

		if (this.denyList == null) {
			throw new PrivilegeException("denyList is null");
		}
		if (this.allowList == null) {
			throw new PrivilegeException("allowList is null");
		}
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * @param name
	 *            the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the policy
	 */
	public String getPolicy() {
		return this.policy;
	}

	/**
	 * @param policy
	 *            the policy to set
	 */
	public void setPolicy(String policy) {
		this.policy = policy;
	}

	/**
	 * @return the allAllowed
	 */
	public boolean isAllAllowed() {
		return this.allAllowed;
	}

	/**
	 * @param allAllowed
	 *            the allAllowed to set
	 */
	public void setAllAllowed(boolean allAllowed) {
		this.allAllowed = allAllowed;
	}

	/**
	 * @return the denyList
	 */
	public Set<String> getDenyList() {
		return this.denyList;
	}

	/**
	 * @param denyList
	 *            the denyList to set
	 */
	public void setDenyList(Set<String> denyList) {
		this.denyList = denyList;
	}

	/**
	 * @return the allowList
	 */
	public Set<String> getAllowList() {
		return this.allowList;
	}

	/**
	 * @param allowList
	 *            the allowList to set
	 */
	public void setAllowList(Set<String> allowList) {
		this.allowList = allowList;
	}

	/**
	 * Returns a string representation of this object displaying its concrete type and its values
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("PrivilegeRep [name=");
		builder.append(this.name);
		builder.append(", policy=");
		builder.append(this.policy);
		builder.append(", allAllowed=");
		builder.append(this.allAllowed);
		builder.append(", denyList=");
		builder.append((this.denyList == null ? "null" : this.denyList.size()));
		builder.append(", allowList=");
		builder.append((this.allowList == null ? "null" : this.allowList.size()));
		builder.append("]");
		return builder.toString();
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
		PrivilegeRep other = (PrivilegeRep) obj;
		if (this.name == null) {
			if (other.name != null)
				return false;
		} else if (!this.name.equals(other.name))
			return false;
		return true;
	}
}
