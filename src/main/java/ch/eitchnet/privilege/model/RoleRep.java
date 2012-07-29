/*
 * Copyright (c) 2010 - 2012
 * 
 * This file is part of Privilege.
 *
 * Privilege is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Privilege is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Privilege.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.privilege.model;

import java.io.Serializable;
import java.util.Map;

import ch.eitchnet.privilege.model.internal.Role;

/**
 * To keep certain details of the {@link Role} itself hidden from remote clients and make sure instances are only edited
 * by users with the correct privilege, this representational version is allowed to be viewed by remote clients and
 * simply wraps all public data from the {@link Role}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RoleRep implements Serializable {

	private static final long serialVersionUID = 1L;

	private String name;
	private Map<String, PrivilegeRep> privilegeMap;

	/**
	 * Default constructor
	 * 
	 * @param name
	 *            the name of this role
	 * @param privilegeMap
	 *            the map of privileges granted to this role
	 */
	public RoleRep(String name, Map<String, PrivilegeRep> privilegeMap) {
		this.name = name;
		this.privilegeMap = privilegeMap;
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
	 * @return the privilegeMap
	 */
	public Map<String, PrivilegeRep> getPrivilegeMap() {
		return this.privilegeMap;
	}

	/**
	 * @param privilegeMap
	 *            the privilegeMap to set
	 */
	public void setPrivileges(Map<String, PrivilegeRep> privilegeMap) {
		this.privilegeMap = privilegeMap;
	}
}
