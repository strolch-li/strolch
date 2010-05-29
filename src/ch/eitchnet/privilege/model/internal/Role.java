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
public class Role {

	private final Map<String, Privilege> privilegeMap;

	/**
	 * @param privilegeMap
	 */
	public Role(Map<String, Privilege> privilegeMap) {
		this.privilegeMap = Collections.unmodifiableMap(privilegeMap);
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
