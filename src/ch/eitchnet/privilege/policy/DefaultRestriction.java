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

import ch.eitchnet.privilege.base.PrivilegeContainer;
import ch.eitchnet.privilege.i18n.PrivilegeException;
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.internal.Privilege;
import ch.eitchnet.privilege.model.internal.Role;

/**
 * @author rvonburg
 * 
 */
public class DefaultRestriction implements RestrictionPolicy {

	/**
	 * @see ch.eitchnet.privilege.policy.RestrictionPolicy#actionAllowed(java.lang.String,
	 *      ch.eitchnet.privilege.model.internal.Role, ch.eitchnet.privilege.model.Restrictable)
	 */
	@Override
	public boolean actionAllowed(Role role, Restrictable restrictable) {

		// validate user is not null
		if (role == null)
			throw new PrivilegeException("Role may not be null!");

		// get the restriction key
		String restrictionKey = restrictable.getRestrictionKey();
		if (restrictionKey == null || restrictionKey.isEmpty()) {
			throw new PrivilegeException("The restriction key for the Restrictable is null or empty: " + restrictable);
		}

		// get restriction object for users role
		Privilege privilege = PrivilegeContainer.getInstance().getModelHandler().getPrivilege(restrictionKey);

		// no restriction object means no privilege
		// TODO should default deny/allow policy be configurable?
		if (privilege == null)
			return false;

		// does this role have privilege for any values?
		if (privilege.isAllAllowed())
			return true;

		// get the value on which the action is to be performed
		Object object = restrictable.getRestrictionValue();

		// DefaultRestriction policy expects the restriction value to be a string
		if (!(object instanceof String)) {
			throw new PrivilegeException(Restrictable.class.getName() + " " + restrictable.getClass().getSimpleName()
					+ " has returned a non-string restriction value!");
		}

		String restrictionValue = (String) object;

		// first check values not allowed
		for (String denied : privilege.getDenyList()) {
			if (denied.equals(restrictionValue))
				return false;
		}

		// now check values allowed
		for (String allowed : privilege.getAllowList()) {
			if (allowed.equals(restrictionValue))
				return true;
		}

		// default is not allowed
		return false;
	}
}
