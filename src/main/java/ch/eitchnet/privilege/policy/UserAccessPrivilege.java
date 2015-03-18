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

import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.i18n.PrivilegeMessages;
import ch.eitchnet.privilege.model.IPrivilege;
import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.internal.User;
import ch.eitchnet.utils.collections.Tuple;
import ch.eitchnet.utils.dbc.DBC;

/**
 * This {@link PrivilegePolicy} expects a {@link Tuple} as {@link Restrictable#getPrivilegeValue()} and then depending
 * on the user specific privileges (see {@link PrivilegeHandler}), uses the basic <code>Allow</code> and
 * <code>Deny</code> to detect if the username of the certificate is allowed
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class UserAccessPrivilege implements PrivilegePolicy {

	@Override
	public void validateAction(PrivilegeContext ctx, IPrivilege privilege, Restrictable restrictable) {
		String privilegeName = PrivilegePolicyHelper.preValidate(privilege, restrictable);

		// get the value on which the action is to be performed
		Object object = restrictable.getPrivilegeValue();

		// RoleAccessPrivilege policy expects the privilege value to be a role
		if (!(object instanceof Tuple)) {
			String msg = Restrictable.class.getName()
					+ PrivilegeMessages.getString("Privilege.illegalArgument.nontuple"); //$NON-NLS-1$
			msg = MessageFormat.format(msg, restrictable.getClass().getSimpleName());
			throw new PrivilegeException(msg);
		}

		// if everything is allowed, then no need to carry on
		if (privilege.isAllAllowed())
			return;

		Tuple tuple = (Tuple) object;

		switch (privilegeName) {
		case PrivilegeHandler.PRIVILEGE_GET_USER: {
			User oldUser = tuple.getFirst();
			User newUser = tuple.getSecond();

			DBC.INTERIM.assertNull("For " + privilegeName + " first must be null!", oldUser);
			DBC.INTERIM.assertNotNull("For " + privilegeName + " second must not be null!", newUser);

			String privilegeValue = newUser.getUsername();
			PrivilegePolicyHelper.checkByAllowDenyValues(ctx, privilege, restrictable, privilegeValue);

			break;
		}
		case PrivilegeHandler.PRIVILEGE_ADD_USER: {
			User oldUser = tuple.getFirst();
			User newUser = tuple.getSecond();

			DBC.INTERIM.assertNull("For " + privilegeName + " first must be null!", oldUser);
			DBC.INTERIM.assertNotNull("For " + privilegeName + " second must not be null!", newUser);

			String privilegeValue = newUser.getUsername();
			PrivilegePolicyHelper.checkByAllowDenyValues(ctx, privilege, restrictable, privilegeValue);

			break;
		}
		case PrivilegeHandler.PRIVILEGE_REMOVE_USER: {
			User oldUser = tuple.getFirst();
			User newUser = tuple.getSecond();

			DBC.INTERIM.assertNull("For " + privilegeName + " first must be null!", oldUser);
			DBC.INTERIM.assertNotNull("For " + privilegeName + " second must not be null!", newUser);

			String privilegeValue = newUser.getUsername();
			PrivilegePolicyHelper.checkByAllowDenyValues(ctx, privilege, restrictable, privilegeValue);

			break;
		}
		case PrivilegeHandler.PRIVILEGE_MODIFY_USER: {
			User oldUser = tuple.getFirst();
			User newUser = tuple.getSecond();

			DBC.INTERIM.assertNotNull("For " + privilegeName + " first must not be null!", oldUser);
			DBC.INTERIM.assertNotNull("For " + privilegeName + " second must not be null!", newUser);

			String privilegeValue = newUser.getUsername();
			DBC.INTERIM.assertEquals("oldUser and newUser names must be the same", oldUser.getUsername(),
					privilegeValue);
			PrivilegePolicyHelper.checkByAllowDenyValues(ctx, privilege, restrictable, privilegeValue);

			break;
		}
		case PrivilegeHandler.PRIVILEGE_ADD_ROLE_TO_USER: {
			User user = tuple.getFirst();
			String roleName = tuple.getSecond();

			DBC.INTERIM.assertNotNull("For " + privilegeName + " first must not be null!", user);
			DBC.INTERIM.assertNotNull("For " + privilegeName + " second must not be null!", roleName);

			PrivilegePolicyHelper.checkByAllowDenyValues(ctx, privilege, restrictable, roleName);

			break;
		}
		case PrivilegeHandler.PRIVILEGE_REMOVE_ROLE_FROM_USER: {
			User user = tuple.getFirst();
			String roleName = tuple.getSecond();

			DBC.INTERIM.assertNotNull("For " + privilegeName + " first must not be null!", user);
			DBC.INTERIM.assertNotNull("For " + privilegeName + " second must not be null!", roleName);

			PrivilegePolicyHelper.checkByAllowDenyValues(ctx, privilege, restrictable, roleName);

			break;
		}

		default:
			String msg = Restrictable.class.getName()
					+ PrivilegeMessages.getString("Privilege.userAccessPrivilege.unknownPrivilege"); //$NON-NLS-1$
			msg = MessageFormat.format(msg, privilegeName);
			throw new PrivilegeException(msg);
		}
	}
}
