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

	public final String name;
	public final Set<String> privileges;

	/**
	 * @param name
	 * @param privileges
	 */
	public RoleRep(String name, Set<String> privileges) {
		this.name = name;
		this.privileges = privileges;
	}
}
