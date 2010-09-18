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
		return name;
	}

	/**
	 * @return the policy
	 */
	public String getPolicy() {
		return policy;
	}

	/**
	 * @return the allAllowed
	 */
	public boolean isAllAllowed() {
		return allAllowed;
	}

	/**
	 * @return the allowList
	 */
	public Set<String> getAllowList() {
		return allowList;
	}

	/**
	 * @return the denyList
	 */
	public Set<String> getDenyList() {
		return denyList;
	}

	/**
	 * @return a {@link PrivilegeRep} which is a representation of this object used to serialize and view on clients
	 */
	public PrivilegeRep asPrivilegeRep() {
		return new PrivilegeRep(name, policy, allAllowed, new HashSet<String>(denyList), new HashSet<String>(allowList));
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("Privilege [name=");
		builder.append(name);
		builder.append(", policy=");
		builder.append(policy);
		builder.append(", allAllowed=");
		builder.append(allAllowed);
		builder.append(", denyList=");
		builder.append(denyList);
		builder.append(", allowList=");
		builder.append(allowList);
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
		result = prime * result + (allAllowed ? 1231 : 1237);
		result = prime * result + ((allowList == null) ? 0 : allowList.hashCode());
		result = prime * result + ((denyList == null) ? 0 : denyList.hashCode());
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + ((policy == null) ? 0 : policy.hashCode());
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
		if (allAllowed != other.allAllowed)
			return false;
		if (allowList == null) {
			if (other.allowList != null)
				return false;
		} else if (!allowList.equals(other.allowList))
			return false;
		if (denyList == null) {
			if (other.denyList != null)
				return false;
		} else if (!denyList.equals(other.denyList))
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (policy == null) {
			if (other.policy != null)
				return false;
		} else if (!policy.equals(other.policy))
			return false;
		return true;
	}
}
