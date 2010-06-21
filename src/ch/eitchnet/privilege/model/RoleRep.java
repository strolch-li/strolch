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
public class RoleRep implements Serializable {

	private static final long serialVersionUID = 1L;

	private String name;
	private Set<String> privileges;

	/**
	 * @param name
	 * @param privileges
	 */
	public RoleRep(String name, Set<String> privileges) {
		this.name = name;
		this.privileges = privileges;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name
	 *            the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the privileges
	 */
	public Set<String> getPrivileges() {
		return privileges;
	}

	/**
	 * @param privileges
	 *            the privileges to set
	 */
	public void setPrivileges(Set<String> privileges) {
		this.privileges = privileges;
	}
}
