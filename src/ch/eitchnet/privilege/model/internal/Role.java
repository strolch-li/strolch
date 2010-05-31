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
import java.util.Map;

/**
 * @author rvonburg
 * 
 */
public final class Role {

	private final String roleName;
	private final Map<String, Privilege> privilegeMap;

	/**
	 * @param privilegeMap
	 */
	public Role(String roleName, Map<String, Privilege> privilegeMap) {
		this.roleName = roleName;
		this.privilegeMap = Collections.unmodifiableMap(privilegeMap);
	}

	/**
	 * @return the roleName
	 */
	public String getRoleName() {
		return roleName;
	}

	/**
	 * @param key
	 * @return
	 * @see java.util.Map#get(java.lang.Object)
	 */
	public Privilege getPrivilege(String key) {
		return privilegeMap.get(key);
	}
}
