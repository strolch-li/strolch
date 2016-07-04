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

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.policy.PrivilegePolicy;
import li.strolch.utils.helper.StringHelper;

/**
 * To keep certain details of the {@link IPrivilege} itself hidden from remote clients and make sure instances are only
 * edited by users with the correct privilege, this representational version is allowed to be viewed by remote clients
 * and simply wraps all public data from the {@link IPrivilege}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlRootElement(name = "Privilege")
@XmlAccessorType(XmlAccessType.NONE)
public class PrivilegeRep implements Serializable {

	private static final long serialVersionUID = 1L;

	@XmlAttribute(name = "name")
	private String name;

	@XmlAttribute(name = "policy")
	private String policy;

	@XmlAttribute(name = "allAllowed")
	private boolean allAllowed;

	@XmlElement(name = "denyList")
	private Set<String> denyList;

	@XmlElement(name = "allowList")
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
	}

	/**
	 * 
	 */
	@SuppressWarnings("unused")
	private PrivilegeRep() {
		// no-arg constructor for JAXB
	}

	/**
	 * Validates that all required fields are set
	 */
	public void validate() {

		if (StringHelper.isEmpty(this.name)) {
			throw new PrivilegeException("No name defined!"); //$NON-NLS-1$
		}

		if (StringHelper.isEmpty(this.policy)) {
			throw new PrivilegeException("policy is null!"); //$NON-NLS-1$
		}

		if (this.denyList == null) {
			throw new PrivilegeException("denyList is null"); //$NON-NLS-1$
		}
		if (this.allowList == null) {
			throw new PrivilegeException("allowList is null"); //$NON-NLS-1$
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
		return this.denyList == null ? new HashSet<>() : this.denyList;
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
		return this.allowList == null ? new HashSet<>() : this.allowList;
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
	@SuppressWarnings("nls")
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
