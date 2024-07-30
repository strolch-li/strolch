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
import li.strolch.privilege.model.Privilege;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Restrictable;
import li.strolch.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegePolicyHelper {

	/**
	 * Validates the given values and returns the privilege name
	 *
	 * @param privilege
	 * 		the {@link Privilege}
	 * @param restrictable
	 * 		the {@link Restrictable}
	 *
	 * @return the privilege name
	 *
	 * @throws PrivilegeException
	 * 		if something is wrong
	 */
	public static String preValidate(Privilege privilege, Restrictable restrictable) throws PrivilegeException {
		if (privilege == null)
			throw new PrivilegeException(PrivilegeMessages.getString("Privilege.privilegeNull"));
		if (restrictable == null)
			throw new PrivilegeException(PrivilegeMessages.getString("Privilege.restrictableNull"));

		// get the PrivilegeName
		String privilegeName = restrictable.getPrivilegeName();
		if (StringHelper.isEmpty(privilegeName)) {
			String msg = PrivilegeMessages.getString("Privilege.privilegeNameEmpty");
			throw new PrivilegeException(MessageFormat.format(msg, restrictable));
		}

		// we want the privileges names to match
		if (!privilege.getName().equals(privilegeName)) {
			throw new PrivilegeException(MessageFormat
					.format(PrivilegeMessages.getString("Privilege.illegalArgument.privilegeNameMismatch"),//$NON-NLS-1$
							privilege.getName(), privilegeName));
		}

		return privilegeName;
	}

	/**
	 * Validates privilege is granted by checking first if all is allows, then the deny values, then the allow values.
	 * If the privilegeValue is in the deny list or not in the allow list, then access is denied and the {@link
	 * AccessDeniedException} is thrown
	 *
	 * @param ctx
	 * 		the context
	 * @param privilege
	 * 		the privielge
	 * @param restrictable
	 * 		the restrictable
	 * @param privilegeValue
	 * 		the privilege value
	 * @param assertHasPrivilege
	 * 		if true and the privilege is missing, then an {@link AccessDeniedException} is thrown if privilege, otherwise a
	 * 		false is returned
	 *
	 * @return true if access is allowed, false if not allowed and assertHasPrivilege is false
	 *
	 * @throws AccessDeniedException
	 * 		if access is denied
	 */
	public static boolean checkByAllowDenyValues(PrivilegeContext ctx, Privilege privilege, Restrictable restrictable,
			String privilegeValue, boolean assertHasPrivilege) throws AccessDeniedException {

		// first check values not allowed
		if (privilege.isDenied(privilegeValue))
			return handleAccessDenied(ctx, privilege, restrictable, privilegeValue, assertHasPrivilege);

		// now check values allowed
		if (privilege.isAllowed(privilegeValue))
			return true;

		return handleAccessDenied(ctx, privilege, restrictable, privilegeValue, assertHasPrivilege);
	}

	private static boolean handleAccessDenied(PrivilegeContext ctx, Privilege privilege, Restrictable restrictable,
			String privilegeValue, boolean assertHasPrivilege) {

		if (assertHasPrivilege) {
			String msg = MessageFormat
					.format(PrivilegeMessages.getString("Privilege.accessdenied.noprivilege.value"),
							ctx.getUsername(), privilege.getName(), privilegeValue, restrictable.getClass().getName());

			throw new AccessDeniedException(msg);
		}

		return false;
	}
}
