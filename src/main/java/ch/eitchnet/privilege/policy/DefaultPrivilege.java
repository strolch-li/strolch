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
package ch.eitchnet.privilege.policy;

import ch.eitchnet.privilege.base.AccessDeniedException;
import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.internal.Privilege;
import ch.eitchnet.privilege.model.internal.Role;

/**
 * XXX re-think this implementation...
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultPrivilege implements PrivilegePolicy {

	/**
	 * @see ch.eitchnet.privilege.policy.PrivilegePolicy#actionAllowed(ch.eitchnet.privilege.model.internal.Role,
	 *      ch.eitchnet.privilege.model.internal.Privilege, ch.eitchnet.privilege.model.Restrictable)
	 */
	@Override
	public void actionAllowed(Role role, Privilege privilege, Restrictable restrictable) {

		// validate user is not null
		if (role == null)
			throw new PrivilegeException("Role may not be null!");

		// get the PrivilegeName
		String privilegeName = restrictable.getPrivilegeName();
		if (privilegeName == null || privilegeName.isEmpty()) {
			throw new PrivilegeException("The PrivilegeName for the Restrictable is null or empty: " + restrictable);
		}

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

			// if value in deny list
			if (denied.equals(privilegeValue)) {

				// then throw access denied
				throw new AccessDeniedException("Role " + role.getName() + " does not have Privilege " + privilegeName
						+ " needed for Restrictable " + restrictable.getClass().getName());
			}
		}

		// now check values allowed
		for (String allowed : privilege.getAllowList()) {
			if (allowed.equals(privilegeValue))
				return;
		}

		// default is not allowed
		throw new AccessDeniedException("Role " + role.getName() + " does not have Privilege " + privilegeName
				+ " needed for Restrictable " + restrictable.getClass().getName());
	}
}
