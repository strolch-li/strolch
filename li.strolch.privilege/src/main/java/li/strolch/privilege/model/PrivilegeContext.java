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
 * This context gives access to a logged in user's privilege data e.g. the {@link UserRep}, {@link Certificate} and the
 * user's list of {@link PrivilegeRep}
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

	private UserRep userRep;
	private Certificate certificate;
	private Map<String, IPrivilege> privileges;
	private Map<String, PrivilegePolicy> policies;

	public PrivilegeContext(UserRep userRep, Certificate certificate, Map<String, IPrivilege> privileges,
			Map<String, PrivilegePolicy> policies) {
		this.userRep = userRep;
		this.certificate = certificate;
		this.privileges = Collections.unmodifiableMap(new HashMap<>(privileges));
		this.policies = Collections.unmodifiableMap(new HashMap<>(policies));
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

	public void assertHasPrivilege(String privilegeName) {
		if (!this.privileges.containsKey(privilegeName)) {
			String msg = MessageFormat.format(PrivilegeMessages.getString("Privilege.noprivilege.user"), //$NON-NLS-1$
					userRep.getUsername(), privilegeName);
			throw new AccessDeniedException(msg);
		}
	}

	public IPrivilege getPrivilege(String privilegeName) {
		assertHasPrivilege(privilegeName);
		return this.privileges.get(privilegeName);
	}

	public PrivilegePolicy getPolicy(String policyName) {
		PrivilegePolicy policy = this.policies.get(policyName);
		if (policy == null) {
			String msg = "The PrivilegePolicy {0} does not exist on the PrivilegeContext!"; //$NON-NLS-1$
			throw new PrivilegeException(MessageFormat.format(msg, policyName));
		}
		return policy;
	}

	// 
	// business logic
	//

	/**
	 * Validates if the user for this context has the privilege to access to the given {@link Restrictable}. If the user
	 * has the privilege, then this method returns with no exception and void, if the user does not have the privilege,
	 * then a {@link AccessDeniedException} is thrown.
	 * 
	 * @param restrictable
	 *            the {@link Restrictable} which the user wants to access
	 * 
	 * @throws AccessDeniedException
	 *             if the user does not have access
	 * @throws PrivilegeException
	 *             if there is an internal error due to wrongly configured privileges or programming errors
	 */
	public void validateAction(Restrictable restrictable) throws AccessDeniedException, PrivilegeException {

		// the privilege for the restrictable
		String privilegeName = restrictable.getPrivilegeName();
		IPrivilege privilege = this.privileges.get(privilegeName);
		if (privilege == null) {
			String msg = MessageFormat.format(PrivilegeMessages.getString("Privilege.accessdenied.noprivilege"), //$NON-NLS-1$
					getUsername(), privilegeName, restrictable.getClass().getName());
			throw new AccessDeniedException(msg);
		}

		// get the policy referenced by the restrictable
		String policyName = privilege.getPolicy();
		PrivilegePolicy policy = getPolicy(policyName);

		// delegate to the policy
		policy.validateAction(this, privilege, restrictable);
	}
}
