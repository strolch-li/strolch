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
package li.strolch.privilege.model;

import java.text.MessageFormat;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.i18n.PrivilegeMessages;
import li.strolch.privilege.policy.PrivilegePolicy;

/**
 * <p>
 * This context gives access to a logged in user's privilege data e.g. the {@link UserRep}, {@link Certificate} and the
 * user's list of {@link PrivilegeRep}
 * </p>
 *
 * <p>
 * Note: This is an internal object which is not to be serialized to clients
 * </p>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeContext {

	//
	// object state
	//

	private final UserRep userRep;
	private final Certificate certificate;
	private final Map<String, IPrivilege> privileges;
	private final Map<String, PrivilegePolicy> policies;

	public PrivilegeContext(UserRep userRep, Certificate certificate, Map<String, IPrivilege> privileges,
			Map<String, PrivilegePolicy> policies) {
		this.userRep = userRep;
		this.certificate = certificate;
		this.privileges = Map.copyOf(privileges);
		this.policies = Map.copyOf(policies);
	}

	public UserRep getUserRep() {
		return this.userRep;
	}

	public Certificate getCertificate() {
		return this.certificate;
	}

	public String getUsername() {
		return this.userRep.getUsername();
	}

	public Set<String> getPrivilegeNames() {
		return this.privileges.keySet();
	}

	public void assertHasPrivilege(String privilegeName) throws AccessDeniedException {
		if (!this.privileges.containsKey(privilegeName)) {
			String msg = MessageFormat.format(PrivilegeMessages.getString("Privilege.noprivilege.user"),
					userRep.getUsername(), privilegeName);
			throw new AccessDeniedException(msg);
		}
	}

	public boolean hasRole(String roleName) {
		return this.userRep.hasRole(roleName);
	}

	public void assertHasRole(String roleName) throws AccessDeniedException {
		if (!this.userRep.hasRole(roleName)) {
			String msg = MessageFormat.format(PrivilegeMessages.getString("Privilege.noprivilege.role"),
					userRep.getUsername(), roleName);
			throw new AccessDeniedException(msg);
		}
	}

	public void assertHasAnyRole(String... roleNames) throws AccessDeniedException {
		for (String roleName : roleNames) {
			if (this.userRep.hasRole(roleName))
				return;
		}

		String msg = MessageFormat.format(PrivilegeMessages.getString("Privilege.noprivilege.role"),
				userRep.getUsername(), String.join(", ", roleNames));
		throw new AccessDeniedException(msg);
	}

	public boolean hasAnyRole(String... roleNames) throws AccessDeniedException {
		for (String roleName : roleNames) {
			if (this.userRep.hasRole(roleName))
				return true;
		}

		return false;
	}

	public IPrivilege getPrivilege(String privilegeName) throws AccessDeniedException {
		assertHasPrivilege(privilegeName);
		return this.privileges.get(privilegeName);
	}

	public PrivilegePolicy getPolicy(String policyName) throws PrivilegeException {
		PrivilegePolicy policy = this.policies.get(policyName);
		if (policy == null) {
			String msg = "The PrivilegePolicy {0} does not exist on the PrivilegeContext!";
			throw new PrivilegeException(MessageFormat.format(msg, policyName));
		}
		return policy;
	}

	//
	// business logic
	//

	/**
	 * <p>Validates if the user for this context has the Privilege with the given name, and is allowed access to the
	 * given value. If the user has the privilege, then this method returns with no exception and void, if the user does
	 * not have the privilege, then a {@link AccessDeniedException} is thrown.</p>
	 *
	 * <p>This method uses the {@link SimpleRestrictable} to verify access</p>
	 *
	 * @param privilegeName
	 * 		the name of the privilege to verify
	 * @param privilegeValue
	 * 		the value
	 *
	 * @throws AccessDeniedException
	 * 		if the user does not have access
	 * @throws PrivilegeException
	 * 		if there is an internal error due to wrongly configured privileges or programming errors
	 */
	public void validateAction(String privilegeName, String privilegeValue)
			throws PrivilegeException, AccessDeniedException {
		validateAction(new SimpleRestrictable(privilegeName, privilegeValue));
	}

	/**
	 * Validates if the user for this context has the privilege to access to the given {@link Restrictable}. If the user
	 * has the privilege, then this method returns with no exception and void, if the user does not have the privilege,
	 * then a {@link AccessDeniedException} is thrown.
	 *
	 * @param restrictable
	 * 		the {@link Restrictable} which the user wants to access
	 *
	 * @throws AccessDeniedException
	 * 		if the user does not have access
	 * @throws PrivilegeException
	 * 		if there is an internal error due to wrongly configured privileges or programming errors
	 */
	public void validateAction(Restrictable restrictable) throws PrivilegeException, AccessDeniedException {

		// the privilege for the restrictable
		String privilegeName = restrictable.getPrivilegeName();
		IPrivilege privilege = this.privileges.get(privilegeName);
		if (privilege == null) {
			String msg = MessageFormat
					.format(PrivilegeMessages.getString("Privilege.accessdenied.noprivilege"),
							getUsername(), privilegeName, restrictable.getClass().getName(),
							restrictable.getPrivilegeValue());
			throw new AccessDeniedException(msg);
		}

		// get the policy referenced by the restrictable
		String policyName = privilege.getPolicy();
		PrivilegePolicy policy = getPolicy(policyName);

		// delegate to the policy
		policy.validateAction(this, privilege, restrictable);
	}

	/**
	 * Validates if the user for this context has the privilege to access to the given {@link Restrictable}. Returning
	 * true if the user has the privilege, and false if not
	 *
	 * @param restrictable
	 * 		the {@link Restrictable} which the user wants to access
	 *
	 * @return returns true if the user has the privilege, and false if not
	 *
	 * @throws PrivilegeException
	 * 		if there is an internal error due to wrongly configured privileges or programming errors
	 */
	public boolean hasPrivilege(Restrictable restrictable) throws PrivilegeException {

		// the privilege for the restrictable
		String privilegeName = restrictable.getPrivilegeName();
		IPrivilege privilege = this.privileges.get(privilegeName);
		if (privilege == null)
			return false;

		// get the policy referenced by the restrictable
		String policyName = privilege.getPolicy();
		PrivilegePolicy policy = getPolicy(policyName);

		// delegate to the policy
		return policy.hasPrivilege(this, privilege, restrictable);
	}

	/**
	 * Validates if the user for this context has the privilege to access to the given {@link Restrictable}. Returning
	 * true if the user has the privilege, and false if not
	 *
	 * <p>This method uses the {@link SimpleRestrictable} to verify access</p>
	 *
	 * @param privilegeName
	 * 		the name of the privilege to verify
	 * @param privilegeValue
	 * 		the value
	 *
	 * @return returns true if the user has the privilege, and false if not
	 *
	 * @throws PrivilegeException
	 * 		if there is an internal error due to wrongly configured privileges or programming errors
	 */
	public boolean hasPrivilege(String privilegeName, String privilegeValue) throws PrivilegeException {
		return hasPrivilege(new SimpleRestrictable(privilegeName, privilegeValue));
	}
}
