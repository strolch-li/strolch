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

import java.text.MessageFormat;

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.i18n.PrivilegeMessages;
import li.strolch.privilege.model.IPrivilege;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Restrictable;
import li.strolch.privilege.model.internal.Role;

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
	 * @see li.strolch.privilege.policy.PrivilegePolicy#validateAction(PrivilegeContext, IPrivilege, Restrictable)
	 */
	@Override
	public void validateAction(PrivilegeContext ctx, IPrivilege privilege, Restrictable restrictable)
			throws AccessDeniedException {

		String privilegeValue = validatePrivilegeValue(privilege, restrictable);

		// if everything is allowed, then no need to carry on
		if (privilege.isAllAllowed())
			return;

		checkByAllowDenyValues(ctx, privilege, restrictable, privilegeValue, true);
	}

	@Override
	public boolean hasPrivilege(PrivilegeContext ctx, IPrivilege privilege, Restrictable restrictable) {

		String privilegeValue = validatePrivilegeValue(privilege, restrictable);

		// if everything is allowed, then no need to carry on
		if (privilege.isAllAllowed())
			return true;

		return checkByAllowDenyValues(ctx, privilege, restrictable, privilegeValue, false);
	}

	private String validatePrivilegeValue(IPrivilege privilege, Restrictable restrictable) {
		PrivilegePolicyHelper.preValidate(privilege, restrictable);

		// get the value on which the action is to be performed
		Object object = restrictable.getPrivilegeValue();

		// DefaultPrivilege policy expects the privilege value to be a string
		if (!(object instanceof String)) {
			String msg = Restrictable.class.getName() + PrivilegeMessages
					.getString("Privilege.illegalArgument.nonstring");
			msg = MessageFormat.format(msg, restrictable.getClass().getSimpleName());
			throw new PrivilegeException(msg);
		}

		return (String) object;
	}
}
