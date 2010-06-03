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
import java.util.Set;

/**
 * @author rvonburg
 * 
 */
public final class Role {

	private final String roleName;
	private final Set<String> privileges;

	/**
	 * @param privilegeMap
	 */
	public Role(String roleName, Set<String> privileges) {
		this.roleName = roleName;
		this.privileges = Collections.unmodifiableSet(privileges);
	}

	/**
	 * @return the roleName
	 */
	public String getRoleName() {
		return roleName;
	}

	/**
	 * @return
	 */
	public Set<String> getPrivileges() {
		return privileges;
	}

	/**
	 * @param key
	 * @return
	 */
	public boolean hasPrivilege(String key) {
		return privileges.contains(key);
	}
}
