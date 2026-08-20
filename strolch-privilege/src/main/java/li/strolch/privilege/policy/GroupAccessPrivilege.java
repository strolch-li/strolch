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
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.i18n.PrivilegeMessages;
import li.strolch.privilege.model.Group;
import li.strolch.privilege.model.Privilege;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Restrictable;
import li.strolch.utils.collections.Tuple;
import li.strolch.utils.dbc.DBC;

import java.text.MessageFormat;

import static li.strolch.privilege.policy.PrivilegePolicyHelper.checkByAllowDenyValues;
import static li.strolch.privilege.policy.PrivilegePolicyHelper.preValidate;

/**
 * This {@link PrivilegePolicy} expects a {@link Tuple} as {@link Restrictable#getPrivilegeValue()}. The Tuple must
 * contain {@link Group} as first and second value. Then the policy decides depending on the user specific privileges
 * (see {@link PrivilegeHandler}), uses the basic <code>Allow</code> and <code>Deny</code> to detect if access is
 * granted
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class GroupAccessPrivilege extends PrivilegePolicy {

	@Override
	public void validateAction(PrivilegeContext ctx, Privilege privilege, Restrictable restrictable)
			throws AccessDeniedException {
		validateAction(ctx, privilege, restrictable, true);
	}

	@Override
	public boolean hasPrivilege(PrivilegeContext ctx, Privilege privilege, Restrictable restrictable)
			throws PrivilegeException {
		return validateAction(ctx, privilege, restrictable, false);
	}

	private boolean validateAction(PrivilegeContext ctx, Privilege privilege, Restrictable restrictable,
			boolean assertHasPrivilege) throws AccessDeniedException {

		String privilegeName = preValidate(privilege, restrictable);

		// get the value on which the action is to be performed
		Object object = restrictable.getPrivilegeValue();

		// if the object is null, then the validation is only that the privilege must exist
		if (object == null)
			return true;

		// GroupAccessPrivilege policy expects the privilege value to be a group
		if (!(object instanceof Tuple tuple)) {
			String msg = Restrictable.class.getName() + PrivilegeMessages.getString(
					"Privilege.illegalArgument.nontuple");
			msg = MessageFormat.format(msg, restrictable.getClass().getSimpleName());
			throw new PrivilegeException(msg);
		}

		// if everything is allowed, then no need to carry on
		if (privilege.isAllAllowed())
			return true;

		// get group name as privilege value
		String oldGroup = tuple.getFirst() instanceof Group r ? r.name() : tuple.getFirst();
		String newGroup = tuple.getSecond() instanceof Group r ? r.name() : tuple.getSecond();

		switch (privilegeName) {
			case PrivilegeHandler.PRIVILEGE_GET_GROUP, PrivilegeHandler.PRIVILEGE_ADD_GROUP,
				 PrivilegeHandler.PRIVILEGE_REMOVE_GROUP -> {
				DBC.INTERIM.assertNull("For " + privilegeName + " first must be null!", oldGroup);
				DBC.INTERIM.assertNotNull("For " + privilegeName + " second must not be null!", newGroup);

				return checkByAllowDenyValues(ctx, privilege, restrictable, newGroup, assertHasPrivilege);
			}
			case PrivilegeHandler.PRIVILEGE_MODIFY_GROUP -> {
				DBC.INTERIM.assertNotNull("For " + privilegeName + " first must not be null!", oldGroup);
				DBC.INTERIM.assertNotNull("For " + privilegeName + " second must not be null!", newGroup);

				DBC.INTERIM.assertEquals("oldGroup and newGroup names must be the same", oldGroup, newGroup);

				return checkByAllowDenyValues(ctx, privilege, restrictable, newGroup, assertHasPrivilege);
			}
			default -> {
				String msg = Restrictable.class.getName() + PrivilegeMessages.getString(
						"Privilege.groupAccessPrivilege.unknownPrivilege");
				msg = MessageFormat.format(msg, privilegeName);
				throw new PrivilegeException(msg);
			}
		}
	}
}
