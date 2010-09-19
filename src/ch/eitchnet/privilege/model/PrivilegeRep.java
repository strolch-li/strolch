/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.model;

import java.io.Serializable;
import java.util.Set;

/**
 * @author rvonburg
 * 
 */
public class PrivilegeRep implements Serializable {

	private static final long serialVersionUID = 1L;

	private String name;
	private String policy;
	private boolean allAllowed;
	private Set<String> denyList;
	private Set<String> allowList;

	/**
	 * @param name
	 * @param policy
	 * @param allAllowed
	 * @param denyList
	 * @param allowList
	 */
	public PrivilegeRep(String name, String policy, boolean allAllowed, Set<String> denyList, Set<String> allowList) {
		this.name = name;
		this.policy = policy;
		this.allAllowed = allAllowed;
		this.denyList = denyList;
		this.allowList = allowList;
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
}
