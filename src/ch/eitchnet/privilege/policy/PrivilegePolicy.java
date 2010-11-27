/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.policy;

import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.internal.Privilege;
import ch.eitchnet.privilege.model.internal.Role;

/**
 * @author rvonburg
 * 
 */
public interface PrivilegePolicy {

	/**
	 * @param role
	 * @param privilege
	 * @param restrictable
	 * 
	 * @return return true if the action is allowed, false if not
	 */
	public boolean actionAllowed(Role role, Privilege privilege, Restrictable restrictable);
}
