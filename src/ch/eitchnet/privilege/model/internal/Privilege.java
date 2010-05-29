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
public class Privilege {

	private final boolean allAllowed;
	private final List<String> allowList;
	private final List<String> denyList;

	/**
	 * @param allAllowed
	 * @param allowList
	 * @param denyList
	 */
	public Privilege(boolean allAllowed, List<String> allowList, List<String> denyList) {
		this.allAllowed = allAllowed;
		this.allowList = Collections.unmodifiableList(allowList);
		this.denyList = Collections.unmodifiableList(denyList);
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
