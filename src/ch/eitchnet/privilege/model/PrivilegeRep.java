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

import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.model.internal.Privilege;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.policy.PrivilegePolicy;

/**
 * To keep certain details of the {@link Privilege} itself hidden from remote clients and make sure instances are only
 * edited by users with the correct privilege, this representational version is allowed to be viewed by remote clients
 * and simply wraps all public data from the {@link Privilege}
 * 
 * @author rvonburg
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
	 *            a boolean defining if a {@link Role} with this {@link Privilege} has unrestricted access to a
	 *            {@link Restrictable}
	 * @param denyList
	 *            a list of deny rules for this {@link Privilege}
	 * @param allowList
	 *            a list of allow rules for this {@link Privilege}
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
