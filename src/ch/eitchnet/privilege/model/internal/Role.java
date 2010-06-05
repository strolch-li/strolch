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

	private final String name;
	private final Set<String> privileges;

	/**
	 * 
	 * @param name
	 * @param privileges
	 */
	public Role(String name, Set<String> privileges) {
		this.name = name;
		this.privileges = Collections.unmodifiableSet(privileges);
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
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
