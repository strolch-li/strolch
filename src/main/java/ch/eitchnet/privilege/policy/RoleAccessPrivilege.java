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
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.utils.collections.Tuple;
import ch.eitchnet.utils.dbc.DBC;

/**
 * TODO
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RoleAccessPrivilege implements PrivilegePolicy {

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

		// get role name as privilege value
		Role oldRole = tuple.getFirst();
		Role newRole = tuple.getSecond();

		switch (privilegeName) {
		case PrivilegeHandler.PRIVILEGE_GET_ROLE: {
			DBC.INTERIM.assertNull("For " + privilegeName + " first must be null!", oldRole);
			DBC.INTERIM.assertNotNull("For " + privilegeName + " second must not be null!", newRole);

			String privilegeValue = newRole.getName();
			PrivilegePolicyHelper.checkByAllowDenyValues(ctx, privilege, restrictable, privilegeValue);

			break;
		}

		case PrivilegeHandler.PRIVILEGE_ADD_ROLE: {
			DBC.INTERIM.assertNull("For " + privilegeName + " first must be null!", oldRole);
			DBC.INTERIM.assertNotNull("For " + privilegeName + " second must not be null!", newRole);

			String privilegeValue = newRole.getName();
			PrivilegePolicyHelper.checkByAllowDenyValues(ctx, privilege, restrictable, privilegeValue);

			break;
		}
		case PrivilegeHandler.PRIVILEGE_MODIFY_ROLE: {
			DBC.INTERIM.assertNotNull("For " + privilegeName + " first must not be null!", oldRole);
			DBC.INTERIM.assertNotNull("For " + privilegeName + " second must not be null!", newRole);

			String privilegeValue = newRole.getName();
			DBC.INTERIM.assertEquals("oldRole and newRole names must be the same", oldRole.getName(), privilegeValue);

			PrivilegePolicyHelper.checkByAllowDenyValues(ctx, privilege, restrictable, privilegeValue);

			break;
		}
		case PrivilegeHandler.PRIVILEGE_REMOVE_ROLE: {
			DBC.INTERIM.assertNull("For " + privilegeName + " first must be null!", oldRole);
			DBC.INTERIM.assertNotNull("For " + privilegeName + " second must not be null!", newRole);

			String privilegeValue = newRole.getName();
			PrivilegePolicyHelper.checkByAllowDenyValues(ctx, privilege, restrictable, privilegeValue);

			break;
		}

		default:
			String msg = Restrictable.class.getName()
					+ PrivilegeMessages.getString("Privilege.roleAccessPrivilege.unknownPrivilege"); //$NON-NLS-1$
			msg = MessageFormat.format(msg, privilegeName);
			throw new PrivilegeException(msg);
		}
	}
}
