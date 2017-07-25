/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.i18n.PrivilegeMessages;
import li.strolch.privilege.model.IPrivilege;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Restrictable;
import li.strolch.privilege.model.internal.User;
import li.strolch.utils.collections.Tuple;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;

/**
 * Validates that any access to a privilege User is done only by users in the same organisation
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class UserAccessWithSameOrganisationPrivilege extends UserAccessPrivilege {

	private static final String PARAM_ORGANISATION = "organisation";

	@Override
	public void validateAction(PrivilegeContext ctx, IPrivilege privilege, Restrictable restrictable)
			throws AccessDeniedException {
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

		// get user organisation
		String userOrg = ctx.getCertificate().getProperty(PARAM_ORGANISATION);
		if (StringHelper.isEmpty(userOrg)) {
			throw new AccessDeniedException("No organisation configured for user " + ctx.getUsername());
		}

		Tuple tuple = (Tuple) object;

		switch (privilegeName) {
		case PrivilegeHandler.PRIVILEGE_GET_USER:
		case PrivilegeHandler.PRIVILEGE_ADD_USER:
		case PrivilegeHandler.PRIVILEGE_MODIFY_USER:
		case PrivilegeHandler.PRIVILEGE_REMOVE_USER: {

			// make sure old user has same organisation
			User oldUser = tuple.getFirst();
			if (oldUser != null) {
				String oldOrg = oldUser.getProperty(PARAM_ORGANISATION);
				if (!userOrg.equals(oldOrg)) {
					throw new AccessDeniedException("User " + ctx.getUsername()
							+ " may not access users outside of their organisation: " + userOrg + " / " + oldOrg);
				}
			}

			// make sure new user has same organisation
			User newUser = tuple.getSecond();
			DBC.INTERIM.assertNotNull("For " + privilegeName + " second must not be null!", newUser);
			String newdOrg = newUser.getProperty(PARAM_ORGANISATION);
			if (!userOrg.equals(newdOrg)) {
				throw new AccessDeniedException("User " + ctx.getUsername()
						+ " may not access users outside of their organisations: " + userOrg + " / " + newdOrg);
			}

			break;
		}
		case PrivilegeHandler.PRIVILEGE_ADD_ROLE_TO_USER:
		case PrivilegeHandler.PRIVILEGE_REMOVE_ROLE_FROM_USER: {

			User user = tuple.getFirst();
			DBC.INTERIM.assertNotNull("For " + privilegeName + " first must not be null!", user);
			String org = user.getProperty(PARAM_ORGANISATION);
			if (!userOrg.equals(org)) {
				throw new AccessDeniedException("User " + ctx.getUsername()
						+ " may not access users outside of their organisation: " + userOrg + " / " + org);
			}

			break;
		}

		default:
			String msg = Restrictable.class.getName()
					+ PrivilegeMessages.getString("Privilege.userAccessPrivilege.unknownPrivilege"); //$NON-NLS-1$
			msg = MessageFormat.format(msg, privilegeName);
			throw new PrivilegeException(msg);
		}

		// now delegate the rest of the validation to the super class
		super.validateAction(ctx, privilege, restrictable);
	}
}
