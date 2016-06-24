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
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.IPrivilege;
import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.privilege.model.Restrictable;

/**
 * <p>
 * This {@link PrivilegePolicy} expects a {@link Certificate} as {@link Restrictable#getPrivilegeValue()} and uses the
 * basic <code>Allow</code> and <code>Deny</code> to detect if the username of the certificate is allowed.
 * </p>
 * 
 * <p>
 * The {@link Certificate} as privilegeValue is not to be confused with the {@link Certificate} of the current user.
 * This certificate is of the user to which access is request, i.e. modifying the session of a logged in user.
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class UsernameFromCertificatePrivilege implements PrivilegePolicy {

	@Override
	public void validateAction(PrivilegeContext ctx, IPrivilege privilege, Restrictable restrictable) {
		PrivilegePolicyHelper.preValidate(privilege, restrictable);

		// get the value on which the action is to be performed
		Object object = restrictable.getPrivilegeValue();

		// RoleAccessPrivilege policy expects the privilege value to be a role
		if (!(object instanceof Certificate)) {
			String msg = Restrictable.class.getName()
					+ PrivilegeMessages.getString("Privilege.illegalArgument.noncertificate"); //$NON-NLS-1$
			msg = MessageFormat.format(msg, restrictable.getClass().getSimpleName());
			throw new PrivilegeException(msg);
		}

		// if everything is allowed, then no need to carry on
		if (privilege.isAllAllowed())
			return;

		Certificate cert = (Certificate) object;
		String privilegeValue = cert.getUsername();
		PrivilegePolicyHelper.checkByAllowDenyValues(ctx, privilege, restrictable, privilegeValue);
	}
}
