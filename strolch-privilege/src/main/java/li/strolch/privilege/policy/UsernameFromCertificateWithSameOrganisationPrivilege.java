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

import static java.util.stream.Collectors.toSet;
import static li.strolch.privilege.base.PrivilegeConstants.ROLE_STROLCH_ADMIN;
import static li.strolch.privilege.policy.PrivilegePolicyHelper.preValidate;
import static li.strolch.utils.helper.StringHelper.isEmpty;

import java.text.MessageFormat;
import java.util.Set;
import java.util.stream.Stream;

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.i18n.PrivilegeMessages;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.Privilege;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Restrictable;

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

	@Override
	protected boolean validateAction(PrivilegeContext ctx, Privilege privilege, Restrictable restrictable,
			boolean assertHasPrivilege) throws AccessDeniedException {

		preValidate(privilege, restrictable);

		// get the value on which the action is to be performed
		Object object = restrictable.getPrivilegeValue();

		// RoleAccessPrivilege policy expects the privilege value to be a role
		if (!(object instanceof Certificate cert)) {
			String msg = Restrictable.class.getName() + PrivilegeMessages.getString(
					"Privilege.illegalArgument.noncertificate");
			msg = MessageFormat.format(msg, restrictable.getClass().getSimpleName());
			throw new PrivilegeException(msg);
		}

		// get object

		// first validate same organisation
		if (!isStrolchAdminAndIgnoreOrganisation(cert) && !assertUserInSameOrganisation(ctx, cert, assertHasPrivilege))
			return false;

		// now delegate the rest of the validation to the super class
		return super.validateAction(ctx, privilege, restrictable, assertHasPrivilege);
	}

	protected boolean isStrolchAdminAndIgnoreOrganisation(Certificate cert) {
		return cert.hasRole(ROLE_STROLCH_ADMIN);
	}

	protected boolean assertUserInSameOrganisation(PrivilegeContext ctx, Certificate cert, boolean assertHasPrivilege) {
		Set<String> userOrgs = getUserOrganisations(ctx.getCertificate());
		Set<String> orgs = getUserOrganisations(cert);

		if (isUserInOrganisation(userOrgs, orgs))
			return true;

		if (assertHasPrivilege)
			throw new AccessDeniedException(
					"User " + ctx.getUsername() + " may not access users outside of their organisation: " + userOrgs
							+ " / " + orgs);

		return false;
	}

	protected boolean isUserInOrganisation(Set<String> organisations, Set<String> userOrg) {
		return userOrg.stream().anyMatch(organisations::contains);
	}

	protected Set<String> getUserOrganisations(Certificate cert) {
		String userOrg = cert.getOrganisation();
		if (isEmpty(userOrg))
			throw new PrivilegeException("No organisation configured for user " + cert.getUsername());
		return Stream.of(userOrg.split(",")).map(String::trim).collect(toSet());
	}
}
