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

import ch.eitchnet.privilege.model.PrivilegeRep;

/**
 * @author rvonburg
 * 
 */
public final class Privilege {

	private final String name;
	private final String policy;
	private final boolean allAllowed;
	private final Set<String> denyList;
	private final Set<String> allowList;

	/**
	 * 
	 * @param name
	 * @param policy
	 * @param allAllowed
	 * @param denyList
	 * @param allowList
	 */
	public Privilege(String name, String policy, boolean allAllowed, Set<String> denyList, Set<String> allowList) {
		this.name = name;
		this.policy = policy;
		this.allAllowed = allAllowed;
		this.denyList = Collections.unmodifiableSet(denyList);
		this.allowList = Collections.unmodifiableSet(allowList);
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
		builder.append(", allAllowed=");
		builder.append(this.allAllowed);
		builder.append(", denyList=");
		builder.append(this.denyList);
		builder.append(", allowList=");
		builder.append(this.allowList);
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
		result = prime * result + (this.allAllowed ? 1231 : 1237);
		result = prime * result + ((this.allowList == null) ? 0 : this.allowList.hashCode());
		result = prime * result + ((this.denyList == null) ? 0 : this.denyList.hashCode());
		result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
		result = prime * result + ((this.policy == null) ? 0 : this.policy.hashCode());
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
		if (this.allAllowed != other.allAllowed)
			return false;
		if (this.allowList == null) {
			if (other.allowList != null)
				return false;
		} else if (!this.allowList.equals(other.allowList))
			return false;
		if (this.denyList == null) {
			if (other.denyList != null)
				return false;
		} else if (!this.denyList.equals(other.denyList))
			return false;
		if (this.name == null) {
			if (other.name != null)
				return false;
		} else if (!this.name.equals(other.name))
			return false;
		if (this.policy == null) {
			if (other.policy != null)
				return false;
		} else if (!this.policy.equals(other.policy))
			return false;
		return true;
	}
}
