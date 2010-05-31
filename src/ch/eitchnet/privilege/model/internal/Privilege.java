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
import java.util.List;

/**
 * @author rvonburg
 * 
 */
public final class Privilege {

	private final String name;
	private final String policy;
	private final boolean allAllowed;
	private final List<String> denyList;
	private final List<String> allowList;

	/**
	 * @param allAllowed
	 * @param denyList
	 * @param allowList
	 */
	public Privilege(String name, String policy, boolean allAllowed, List<String> denyList, List<String> allowList) {
		this.name = name;
		this.policy = policy;
		this.allAllowed = allAllowed;
		this.denyList = Collections.unmodifiableList(denyList);
		this.allowList = Collections.unmodifiableList(allowList);
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
	public List<String> getAllowList() {
		return allowList;
	}

	/**
	 * @return the denyList
	 */
	public List<String> getDenyList() {
		return denyList;
	}

}
