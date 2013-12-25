/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
			String msg = MessageFormat.format(PrivilegeMessages.getString("Privilege.accessdenied.noprivilege"), //$NON-NLS-1$
					PrivilegeContext.get().getUsername(), privilegeName, restrictable.getClass().getName());
			throw new AccessDeniedException(msg);
		}

		// now check values allowed
		if (privilege.isAllowed(privilegeValue))
			return;

		// default is not allowed
		String msg = MessageFormat.format(PrivilegeMessages.getString("Privilege.accessdenied.noprivilege"), //$NON-NLS-1$
				PrivilegeContext.get().getUsername(), privilegeName, restrictable.getClass().getName());
		throw new AccessDeniedException(msg);
	}
}
