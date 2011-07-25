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

import ch.eitchnet.privilege.i18n.PrivilegeException;
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.internal.Privilege;
import ch.eitchnet.privilege.model.internal.Role;

/**
 * XXX re-think this implementation...
 * 
 * @author rvonburg
 */
public class DefaultPrivilege implements PrivilegePolicy {

	/**
	 * @see ch.eitchnet.privilege.policy.PrivilegePolicy#actionAllowed(ch.eitchnet.privilege.model.internal.Role,
	 *      ch.eitchnet.privilege.model.internal.Privilege, ch.eitchnet.privilege.model.Restrictable)
	 */
	@Override
	public boolean actionAllowed(Role role, Privilege privilege, Restrictable restrictable) {

		// validate user is not null
		if (role == null)
			throw new PrivilegeException("Role may not be null!");

		// get the PrivilegeName
		String privilegeName = restrictable.getPrivilegeName();
		if (privilegeName == null || privilegeName.isEmpty()) {
			throw new PrivilegeException("The PrivilegeName for the Restrictable is null or empty: " + restrictable);
		}

		// does this role have privilege for any values?
		if (privilege.isAllAllowed())
			return true;

		// get the value on which the action is to be performed
		Object object = restrictable.getPrivilegeValue();

		// DefaultPrivilege policy expects the privilege value to be a string
		if (!(object instanceof String)) {
			throw new PrivilegeException(Restrictable.class.getName() + " " + restrictable.getClass().getSimpleName()
					+ " has returned a non-string privilege value!");
		}

		String privilegeValue = (String) object;

		// first check values not allowed
		for (String denied : privilege.getDenyList()) {
			if (denied.equals(privilegeValue))
				return false;
		}

		// now check values allowed
		for (String allowed : privilege.getAllowList()) {
			if (allowed.equals(privilegeValue))
				return true;
		}

		// default is not allowed
		return false;
	}
}
