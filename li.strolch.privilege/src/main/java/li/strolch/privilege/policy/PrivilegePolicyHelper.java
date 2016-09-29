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
package li.strolch.privilege.policy;

import java.text.MessageFormat;

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.i18n.PrivilegeMessages;
import li.strolch.privilege.model.IPrivilege;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Restrictable;
import li.strolch.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegePolicyHelper {

	public static String preValidate(IPrivilege privilege, Restrictable restrictable) {
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
			throw new PrivilegeException(
					MessageFormat.format(PrivilegeMessages.getString("Privilege.illegalArgument.privilegeNameMismatch"), //$NON-NLS-1$
							privilege.getName(), privilegeName));
		}

		return privilegeName;
	}

	/**
	 * @param privilege
	 * @param privilegeValue
	 */
	public static void checkByAllowDenyValues(PrivilegeContext ctx, IPrivilege privilege, Restrictable restrictable,
			String privilegeValue) {

		// first check values not allowed
		if (privilege.isDenied(privilegeValue)) {
			// then throw access denied
			String msg = MessageFormat.format(PrivilegeMessages.getString("Privilege.accessdenied.noprivilege"), //$NON-NLS-1$
					ctx.getUsername(), privilege.getName(), privilegeValue, restrictable.getClass().getName());
			throw new AccessDeniedException(msg);
		}

		// now check values allowed
		if (privilege.isAllowed(privilegeValue))
			return;

		// default is not allowed
		String msg = MessageFormat.format(PrivilegeMessages.getString("Privilege.accessdenied.noprivilege"), //$NON-NLS-1$
				ctx.getUsername(), privilege.getName(), privilegeValue, restrictable.getClass().getName());
		throw new AccessDeniedException(msg);
	}
}
