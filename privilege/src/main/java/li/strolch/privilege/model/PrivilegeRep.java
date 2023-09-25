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

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.policy.PrivilegePolicy;

import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

import static li.strolch.utils.helper.StringHelper.isEmpty;
import static li.strolch.utils.helper.StringHelper.trimOrEmpty;

/**
 * To keep certain details of the {@link Privilege} itself hidden from remote clients and make sure instances are only
 * edited by users with the correct privilege, this representational version is allowed to be viewed by remote clients
 * and simply wraps all public data from the {@link Privilege}
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeRep {

	private String name;
	private String policy;
	private boolean allAllowed;
	private Set<String> denyList;
	private Set<String> allowList;

	/**
	 * Default constructor
	 *
	 * @param name       the name of this privilege, which is unique to all privileges known in the
	 *                   {@link PrivilegeHandler}
	 * @param policy     the {@link PrivilegePolicy} configured to evaluate if the privilege is granted
	 * @param allAllowed a boolean defining if a {@link Role} with this {@link Privilege} has unrestricted access to a
	 *                   {@link Restrictable}
	 * @param denyList   a list of deny rules for this {@link Privilege}
	 * @param allowList  a list of allow rules for this {@link Privilege}
	 */
	public PrivilegeRep(String name, String policy, boolean allAllowed, Set<String> denyList, Set<String> allowList) {
		this.name = trimOrEmpty(name);
		this.policy = trimOrEmpty(policy);
		this.allAllowed = allAllowed;
		this.denyList = denyList == null ? Set.of() : Set.copyOf(denyList);
		this.allowList = allowList == null ? Set.of() : Set.copyOf(allowList);
	}

	/**
	 * Validates that all required fields are set
	 */
	public void validate() {
		if (isEmpty(this.name))
			throw new PrivilegeException("No name defined!");
		if (isEmpty(this.policy))
			throw new PrivilegeException("No policy defined!");
		if (this.denyList == null)
			throw new PrivilegeException("denyList is null");
		if (this.allowList == null)
			throw new PrivilegeException("allowList is null");
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * @param name the name to set
	 */
	public void setName(String name) {
		this.name = trimOrEmpty(name);
	}

	/**
	 * @return the policy
	 */
	public String getPolicy() {
		return this.policy;
	}

	/**
	 * @param policy the policy to set
	 */
	public void setPolicy(String policy) {
		this.policy = trimOrEmpty(policy);
	}

	/**
	 * @return the allAllowed
	 */
	public boolean isAllAllowed() {
		return this.allAllowed;
	}

	/**
	 * @param allAllowed the allAllowed to set
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
	 * @param denyList the denyList to set
	 */
	public void setDenyList(Set<String> denyList) {
		this.denyList = denyList.stream().map(String::trim).collect(Collectors.toSet());
	}

	/**
	 * @return the allowList
	 */
	public Set<String> getAllowList() {
		return this.allowList;
	}

	/**
	 * @param allowList the allowList to set
	 */
	public void setAllowList(Set<String> allowList) {
		this.allowList = allowList.stream().map(String::trim).collect(Collectors.toSet());
	}

	/**
	 * Returns a string representation of this object displaying its concrete type and its values
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "PrivilegeRep [name=" + this.name + ", policy=" + this.policy + ", allAllowed=" + this.allAllowed +
				", denyList=" + this.denyList.size() + ", allowList=" + this.allowList.size() + "]";
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
		if (this.name == null)
			return other.name == null;
		return this.name.equals(other.name);
	}

	public <T> T accept(PrivilegeElementVisitor<T> visitor) {
		return visitor.visitPrivilegeRep(this);
	}
}
