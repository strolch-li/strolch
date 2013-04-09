/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package ch.eitchnet.privilege.model;

import java.util.Set;

import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.policy.PrivilegePolicy;

/**
 * <p>
 * {@link IPrivilege} is the main model object for Privilege. A {@link Role} has a set of Privileges assigned to it
 * which defines the privileges a logged in user with that role has. If the {@link IPrivilege} has a
 * {@link PrivilegePolicy} defined, then that policy will be used for finer granularity and with the deny and allow
 * lists configured which is used to evaluate if privilege is granted to a {@link Restrictable}
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public interface IPrivilege {

	/**
	 * @return a {@link PrivilegeRep} which is a representation of this object used to serialize and view on clients
	 */
	public abstract PrivilegeRep asPrivilegeRep();

	/**
	 * @return the name
	 */
	public abstract String getName();

	/**
	 * @return the policy
	 */
	public abstract String getPolicy();

	/**
	 * @return the allAllowed
	 */
	public abstract boolean isAllAllowed();

	/**
	 * @return the allowList
	 */
	public abstract Set<String> getAllowList();

	/**
	 * @return the denyList
	 */
	public abstract Set<String> getDenyList();

	/**
	 * @return true if there are values in the allow list
	 */
	public abstract boolean hasAllowed();

	/**
	 * @return if the value is in the allow list
	 */
	public abstract boolean isAllowed(String value);

	/**
	 * @return true if there are values in the deny list
	 */
	public abstract boolean hasDenied();

	/**
	 * @return true if the value is in the deny list
	 */
	public abstract boolean isDenied(String value);

}