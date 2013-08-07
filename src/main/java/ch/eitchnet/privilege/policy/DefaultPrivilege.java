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

import java.text.MessageFormat;

import ch.eitchnet.privilege.base.AccessDeniedException;
import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.i18n.PrivilegeMessages;
import ch.eitchnet.privilege.model.IPrivilege;
import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * This is a simple implementation of {@link PrivilegePolicy} which uses the {@link Restrictable#getPrivilegeName()} to
 * see if a given {@link Role} has the privilege required by the value from {@link Restrictable#getPrivilegeValue()}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultPrivilege implements PrivilegePolicy {

	/**
	 * The value of {@link Restrictable#getPrivilegeValue()} is used to check if the {@link Role} has this privilege
	 * 
	 * @see ch.eitchnet.privilege.policy.PrivilegePolicy#validateAction(IPrivilege, Restrictable)
	 */
	@Override
	public void validateAction(IPrivilege privilege, Restrictable restrictable) {

		if (privilege == null)
			throw new PrivilegeException(PrivilegeMessages.getString("Privilege.privilegeNull")); //$NON-NLS-1$
		if (restrictable == null)
			throw new PrivilegeException(PrivilegeMessages.getString("Privilege.restrictableNull")); //$NON-NLS-1$

		// get the PrivilegeName
		String privilegeName = restrictable.getPrivilegeName();
		if (StringHelper.isEmpty(privilegeName)) {
			String msg = PrivilegeMessages.getString("Privilege.privilegeNameEmpty"); //$NON-NLS-1$
			throw new PrivilegeException(MessageFormat.format(msg, restrictable));
		}

		// we want the privileges names to match
		if (!privilege.getName().equals(privilegeName)) {
			throw new PrivilegeException(MessageFormat.format(
					PrivilegeMessages.getString("Privilege.illegalArgument.privilegeNameMismatch"), //$NON-NLS-1$
					privilege.getName(), privilegeName));
		}

		// if everything is allowed, then no need to carry on
		if (privilege.isAllAllowed())
			return;

		// get the value on which the action is to be performed
		Object object = restrictable.getPrivilegeValue();

		// DefaultPrivilege policy expects the privilege value to be a string
		if (!(object instanceof String)) {
			String msg = Restrictable.class.getName()
					+ PrivilegeMessages.getString("Privilege.illegalArgument.nonstring"); //$NON-NLS-1$
			msg = MessageFormat.format(msg, restrictable.getClass().getSimpleName());
			throw new PrivilegeException(msg);
		}

		String privilegeValue = (String) object;

		// first check values not allowed
		if (privilege.isDenied(privilegeValue)) {
			// then throw access denied
			String msg = MessageFormat.format(PrivilegeMessages.getString("Privilege.accessdenied.noprivilege"),
					PrivilegeContext.get().getUsername(), privilegeName, restrictable.getClass().getName());
			throw new AccessDeniedException(msg);
		}

		// now check values allowed
		if (privilege.isAllowed(privilegeValue))
			return;

		// default is not allowed
		String msg = MessageFormat.format(PrivilegeMessages.getString("Privilege.accessdenied.noprivilege"),
				PrivilegeContext.get().getUsername(), privilegeName, restrictable.getClass().getName());
		throw new AccessDeniedException(msg);
	}
}
