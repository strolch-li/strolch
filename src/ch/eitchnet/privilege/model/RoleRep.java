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

import ch.eitchnet.privilege.model.internal.Role;

/**
 * To keep certain details of the {@link Role} itself hidden from remote clients and make sure instances are only edited
 * by users with the correct privilege, this representational version is allowed to be viewed by remote clients and
 * simply wraps all public data from the {@link Role}
 * 
 * @author rvonburg
 */
public class RoleRep implements Serializable {

	private static final long serialVersionUID = 1L;

	private String name;
	private Set<String> privileges;

	/**
	 * Default constructor
	 * 
	 * @param name
	 *            the name of this role
	 * @param privileges
	 *            the set of privileges granted to this role
	 */
	public RoleRep(String name, Set<String> privileges) {
		this.name = name;
		this.privileges = privileges;
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
	 * @return the privileges
	 */
	public Set<String> getPrivileges() {
		return this.privileges;
	}

	/**
	 * @param privileges
	 *            the privileges to set
	 */
	public void setPrivileges(Set<String> privileges) {
		this.privileges = privileges;
	}
}
