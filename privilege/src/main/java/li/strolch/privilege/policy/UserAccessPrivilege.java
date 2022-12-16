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

import static li.strolch.privilege.policy.PrivilegePolicyHelper.checkByAllowDenyValues;
import static li.strolch.privilege.policy.PrivilegePolicyHelper.preValidate;

import java.text.MessageFormat;

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.i18n.PrivilegeMessages;
import li.strolch.privilege.model.IPrivilege;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Restrictable;
import li.strolch.privilege.model.internal.User;
import li.strolch.utils.collections.Tuple;
import li.strolch.utils.dbc.DBC;

/**
 * This {@link PrivilegePolicy} expects a {@link Tuple} as {@link Restrictable#getPrivilegeValue()} and then depending
 * on the user specific privileges (see {@link PrivilegeHandler}), uses the basic <code>Allow</code> and
 * <code>Deny</code> to detect if the username of the certificate is allowed
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class UserAccessPrivilege implements PrivilegePolicy {

	@Override
	public void validateAction(PrivilegeContext ctx, IPrivilege privilege, Restrictable restrictable)
			throws AccessDeniedException {
		validateAction(ctx, privilege, restrictable, true);
	}

	@Override
	public boolean hasPrivilege(PrivilegeContext ctx, IPrivilege privilege, Restrictable restrictable)
			throws PrivilegeException {
		return validateAction(ctx, privilege, restrictable, false);
	}

	protected boolean validateAction(PrivilegeContext ctx, IPrivilege privilege, Restrictable restrictable,
			boolean assertHasPrivilege) throws AccessDeniedException {

		String privilegeName = preValidate(privilege, restrictable);

		// get the value on which the action is to be performed
		Object object = restrictable.getPrivilegeValue();

		// if the object is null, then the validation is only that the privilege must exist
		if (object == null)
			return true;

		// RoleAccessPrivilege policy expects the privilege value to be a role
		if (!(object instanceof Tuple)) {
			String msg = Restrictable.class.getName() + PrivilegeMessages
					.getString("Privilege.illegalArgument.nontuple"); //$NON-NLS-1$
			msg = MessageFormat.format(msg, restrictable.getClass().getSimpleName());
			throw new PrivilegeException(msg);
		}

		// if everything is allowed, then no need to carry on
		if (privilege.isAllAllowed())
			return true;

		Tuple tuple = (Tuple) object;

		switch (privilegeName) {
		case PrivilegeHandler.PRIVILEGE_GET_USER:
		case PrivilegeHandler.PRIVILEGE_ADD_USER:
		case PrivilegeHandler.PRIVILEGE_REMOVE_USER:
		case PrivilegeHandler.PRIVILEGE_MODIFY_USER: {
			User oldUser = tuple.getFirst();
			User newUser = tuple.getSecond();

			DBC.INTERIM.assertNotNull("For " + privilegeName + " first must not be null!", oldUser);
			DBC.INTERIM.assertNotNull("For " + privilegeName + " second must not be null!", newUser);

			String privilegeValue = newUser.getUsername();
			DBC.INTERIM
					.assertEquals("oldUser and newUser names must be the same", oldUser.getUsername(), privilegeValue);
			return checkByAllowDenyValues(ctx, privilege, restrictable, privilegeValue, assertHasPrivilege);
		}

		case PrivilegeHandler.PRIVILEGE_SET_USER_STATE: {
			User oldUser = tuple.getFirst();
			User newUser = tuple.getSecond();

			DBC.INTERIM.assertNotNull("For " + privilegeName + " first must not be null!", oldUser);
			DBC.INTERIM.assertNotNull("For " + privilegeName + " second must not be null!", newUser);

			String privilegeValue = newUser.getUserState().name();
			return checkByAllowDenyValues(ctx, privilege, restrictable, privilegeValue, assertHasPrivilege);
		}

		case PrivilegeHandler.PRIVILEGE_SET_USER_LOCALE:
		case PrivilegeHandler.PRIVILEGE_SET_USER_PASSWORD: {
			User oldUser = tuple.getFirst();
			User newUser = tuple.getSecond();

			DBC.INTERIM.assertNotNull("For " + privilegeName + " first must not be null!", oldUser);
			DBC.INTERIM.assertNotNull("For " + privilegeName + " second must not be null!", newUser);

			String privilegeValue = newUser.getUsername();

			// user can set their own locale
			if (ctx.getUsername().equals(privilegeValue))
				return true;

			return checkByAllowDenyValues(ctx, privilege, restrictable, privilegeValue, assertHasPrivilege);
		}

		case PrivilegeHandler.PRIVILEGE_ADD_ROLE_TO_USER:
		case PrivilegeHandler.PRIVILEGE_REMOVE_ROLE_FROM_USER: {
			User user = tuple.getFirst();
			String roleName = tuple.getSecond();

			DBC.INTERIM.assertNotNull("For " + privilegeName + " first must not be null!", user);
			DBC.INTERIM.assertNotNull("For " + privilegeName + " second must not be null!", roleName);

			return checkByAllowDenyValues(ctx, privilege, restrictable, roleName, assertHasPrivilege);
		}

		default:
			String msg = PrivilegeMessages.getString("Privilege.userAccessPrivilege.unknownPrivilege"); //$NON-NLS-1$
			msg = MessageFormat.format(msg, privilegeName, this.getClass().getName());
			throw new PrivilegeException(msg);
		}
	}
}
