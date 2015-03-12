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
import ch.eitchnet.privilege.i18n.PrivilegeMessages;
import ch.eitchnet.privilege.model.IPrivilege;
import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.internal.Role;

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
	public void validateAction(PrivilegeContext ctx, IPrivilege privilege, Restrictable restrictable) {
		PrivilegePolicyHelper.preValidate(privilege, restrictable);

		// get the value on which the action is to be performed
		Object object = restrictable.getPrivilegeValue();

		// DefaultPrivilege policy expects the privilege value to be a string
		if (!(object instanceof String)) {
			String msg = Restrictable.class.getName()
					+ PrivilegeMessages.getString("Privilege.illegalArgument.nonstring"); //$NON-NLS-1$
			msg = MessageFormat.format(msg, restrictable.getClass().getSimpleName());
			throw new PrivilegeException(msg);
		}

		// if everything is allowed, then no need to carry on
		if (privilege.isAllAllowed())
			return;

		String privilegeValue = (String) object;

		PrivilegePolicyHelper.checkByAllowDenyValues(ctx, privilege, restrictable, privilegeValue);
	}
}
