/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.i18n.PrivilegeMessages;
import li.strolch.privilege.model.Privilege;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Restrictable;
import li.strolch.utils.helper.StringHelper;

import java.text.MessageFormat;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegePolicyHelper {

	private PrivilegePolicyHelper() {
	}

	/**
	 * Validates the provided privilege and restrictable objects to ensure their compatibility, and retrieves the
	 * privilege name associated with the restrictable object. This method ensures that the privilege's name matches the
	 * privilege name required by the restrictable object.
	 *
	 * @param privilege    the {@link Privilege} to be validated; must not be null
	 * @param restrictable the {@link Restrictable} to be validated; must not be null
	 *
	 * @return the privilege name associated with the restrictable, if the validation passes
	 *
	 * @throws PrivilegeException if the privilege or restrictable is null, if the privilege name is empty, or if the
	 *                            privilege name does not match the restrictable's required privilege name
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
			throw new PrivilegeException(
					MessageFormat.format(PrivilegeMessages.getString("Privilege.illegalArgument.privilegeNameMismatch"),
							//$NON-NLS-1$
							privilege.getName(), privilegeName));
		}

		return privilegeName;
	}

	/**
	 * Checks whether the specified privilege value is allowed or denied based on the given privilege's configuration.
	 * The method first evaluates denied values, then allowed values. If the privilege value is not explicitly allowed
	 * or denied, it handles access denial based on the given parameters.
	 *
	 * @param ctx                the {@link PrivilegeContext} providing the context of the privilege check
	 * @param privilege          the {@link Privilege} containing the allowed and denied privilege configurations
	 * @param restrictable       the {@link Restrictable} associated with the privilege
	 * @param privilegeValue     the specific privilege value to check
	 * @param assertHasPrivilege a flag indicating whether an exception should be thrown if access is denied
	 *
	 * @return {@code true} if the privilege value is allowed; {@code false} if it is denied and
	 * {@code assertHasPrivilege} is {@code false}
	 *
	 * @throws AccessDeniedException if access is denied and {@code assertHasPrivilege} is {@code true}
	 */
	public static boolean checkByAllowDenyValues(PrivilegeContext ctx, Privilege privilege, Restrictable restrictable,
			String privilegeValue, boolean assertHasPrivilege) throws AccessDeniedException {

		// first check values not allowed
		// now check values allowed
		if (privilege.hasDenied()) {
			if (privilege.isDenied(privilegeValue))
				return handleAccessDenied(ctx, privilege, restrictable, privilegeValue, assertHasPrivilege);
			return true;
		}

		if (privilege.hasAllowed() && privilege.isAllowed(privilegeValue))
			return true;

		return handleAccessDenied(ctx, privilege, restrictable, privilegeValue, assertHasPrivilege);
	}

	private static boolean handleAccessDenied(PrivilegeContext ctx, Privilege privilege, Restrictable restrictable,
			String privilegeValue, boolean assertHasPrivilege) {

		if (assertHasPrivilege) {
			String msg = MessageFormat.format(PrivilegeMessages.getString("Privilege.accessdenied.noprivilege.value"),
					ctx.getUsername(), privilege.getName(), privilegeValue, restrictable.getClass().getName());

			throw new AccessDeniedException(msg);
		}

		return false;
	}
}
