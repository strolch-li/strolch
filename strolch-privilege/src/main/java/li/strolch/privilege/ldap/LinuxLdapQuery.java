/*
 * Copyright (c) 2024-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.privilege.ldap;

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.PrivilegeException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.naming.AuthenticationException;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.directory.DirContext;
import javax.naming.directory.InitialDirContext;
import javax.naming.directory.SearchResult;
import java.util.Hashtable;

import static li.strolch.utils.LdapHelper.encodeForLDAP;

public class LinuxLdapQuery extends LdapQuery {

	private static final Logger logger = LoggerFactory.getLogger(LinuxLdapQuery.class);

	private final LinuxLdapQueryContext queryContext;
	protected InitialDirContext directoryContext;

	public LinuxLdapQuery(LinuxLdapQueryContext queryContext) {
		this.queryContext = queryContext;
	}

	@Override
	public SearchResult searchLdap(String username, char[] password) throws NamingException {
		// escape the user provider username
		String safeUsername = encodeForLDAP(username, true);

		if (this.queryContext.isServiceUserDefined()) {
			return fetchUserDataWithServiceUser(username, password, safeUsername);
		} else {
			return fetchUserData(username, password);
		}
	}

	private SearchResult fetchUserDataWithServiceUser(String username, char[] password, String safeUsername)
			throws NamingException {

		// Step 1: Validate the user's credentials by attempting to bind
		String userDn = this.queryContext.buildUserDn(username);
		logger.info("Checking password for user with DN {}", userDn);
		if (!validateUserPassword(userDn, password))
			throw new AccessDeniedException("Authentication failed for user %s".formatted(safeUsername));

		// Step 2: Fetch additional user data using the service user
		try {
			this.directoryContext = new InitialDirContext(this.queryContext.buildServiceUserLdapEnv());
		} catch (AuthenticationException e) {
			throw new AccessDeniedException("Authentication failed for service user!");
		}
		return fetchUserData(userDn);
	}

	private SearchResult fetchUserData(String username, char[] password) throws NamingException {
		String userDn = this.queryContext.buildUserDn(username);
		logger.info("Logging in with user DN {}", userDn);
		try {
			this.directoryContext = new InitialDirContext(this.queryContext.buildLdapEnv(password, userDn));
		} catch (AuthenticationException e) {
			throw new AccessDeniedException("Authentication failed for user %s".formatted(username));
		}
		return fetchUserData(userDn);
	}

	private SearchResult fetchUserData(String userDn) throws NamingException {
		// Perform the search
		String filter = this.queryContext.getObjectClassFilter();
		NamingEnumeration<SearchResult> results = this.directoryContext.search(userDn, filter, this.searchControls);

		if (!results.hasMore())
			throw new PrivilegeException("No data found for user %s".formatted(userDn));

		return results.next();
	}

	protected boolean validateUserPassword(String userDn, char[] password) {
		Hashtable<String, String> env = this.queryContext.buildLdapEnv(password, userDn);

		DirContext context = null;
		try {
			context = new InitialDirContext(env);
			return true; // If binding succeeds, the password is valid
		} catch (AuthenticationException e) {
			logger.error("Invalid credentials for user DN: {}", userDn);
		} catch (Exception e) {
			logger.error("Error validating user password: {}", e.getMessage(), e);
		} finally {
			if (context != null) {
				try {
					context.close();
				} catch (Exception e) {
					logger.error("Error closing LDAP context: {}", e.getMessage());
				}
			}
		}

		return false;
	}

	@Override
	public void close() {
		if (this.directoryContext != null) {
			try {
				this.directoryContext.close();
			} catch (NamingException e) {
				logger.error("Failed to close context!", e);
			}
		}
	}
}
