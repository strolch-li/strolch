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

import ch.eitchnet.privilege.base.AccessDeniedException;
import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.i18n.PrivilegeMessages;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.IPrivilege;
import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.utils.helper.StringHelper;

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
public class UsernameFromCertificateWithSameOrganisationPrivilege extends UsernameFromCertificatePrivilege {

	private static final String PARAM_ORGANISATION = "organisation";

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

		// get object
		Certificate cert = (Certificate) object;

		// get user organisation
		String userOrg = ctx.getCertificate().getProperty(PARAM_ORGANISATION);
		if (StringHelper.isEmpty(userOrg)) {
			throw new AccessDeniedException("No organisation configured for user " + ctx.getUsername());
		}
		// assert same organisation
		String org = cert.getProperty(PARAM_ORGANISATION);
		if (!userOrg.equals(org)) {
			throw new AccessDeniedException("User " + ctx.getUsername()
					+ " may not access users outside of their organisation: " + userOrg + " / " + org);
		}

		// now delegate the rest of the validation to the super class
		super.validateAction(ctx, privilege, restrictable);
	}
}
