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

package li.strolch.privilege.test;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.naming.AuthenticationException;
import javax.naming.Context;
import javax.naming.NamingEnumeration;
import javax.naming.directory.DirContext;
import javax.naming.directory.InitialDirContext;
import javax.naming.directory.SearchControls;
import javax.naming.directory.SearchResult;
import java.util.Hashtable;

public class LdapQueryExampleTest {

	private static final Logger logger = LoggerFactory.getLogger(LdapQueryExampleTest.class);

	public static void main(String[] args) {
		String ldapUrl = "ldap://localhost:10389";
		String serviceUserDn = "uid=admin,ou=Admins,dc=strolch,dc=li";
		String serviceUserPassword = "admin";
		String userSearchBaseDn = "ou=People,dc=strolch,dc=li";
		String userSearchFilter = "(uid=%s)";

		String username = "test1";
		String userPassword = "test";

		// Step 1: Use the service user to search for the target user's DN
		String userDn = getUserDn(ldapUrl, serviceUserDn, serviceUserPassword, userSearchBaseDn,
				String.format(userSearchFilter, username));

		if (userDn != null) {
			// Step 2: Validate the user's password by binding with their DN
			if (validateUserPassword(ldapUrl, userDn, userPassword)) {
				logger.info("OK: User authentication successful.");
			} else {
				logger.info("FAILED: User authentication failed.");
			}
		} else {
			logger.info("FAILED: User not found.");
		}
	}

	private static String getUserDn(String ldapUrl, String serviceUserDn, String serviceUserPassword, String baseDn,
			String searchFilter) {
		Hashtable<String, String> env = new Hashtable<>();
		env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
		env.put(Context.PROVIDER_URL, ldapUrl);
		env.put(Context.SECURITY_AUTHENTICATION, "simple");
		env.put(Context.SECURITY_PRINCIPAL, serviceUserDn);
		env.put(Context.SECURITY_CREDENTIALS, serviceUserPassword);

		DirContext context = null;
		try {
			context = new InitialDirContext(env);

			SearchControls controls = new SearchControls();
			controls.setSearchScope(SearchControls.SUBTREE_SCOPE);

			NamingEnumeration<SearchResult> results = context.search(baseDn, searchFilter, controls);

			if (results.hasMore()) {
				SearchResult result = results.next();
				return result.getNameInNamespace();
			}
		} catch (Exception e) {
			logger.error("Error fetching user DN: {}", e.getMessage(), e);
		} finally {
			if (context != null) {
				try {
					context.close();
				} catch (Exception e) {
					logger.error("Error closing LDAP context: {}", e.getMessage());
				}
			}
		}
		return null;
	}

	private static boolean validateUserPassword(String ldapUrl, String userDn, String userPassword) {
		Hashtable<String, String> env = new Hashtable<>();
		env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
		env.put(Context.PROVIDER_URL, ldapUrl);
		env.put(Context.SECURITY_AUTHENTICATION, "simple");
		env.put(Context.SECURITY_PRINCIPAL, userDn);
		env.put(Context.SECURITY_CREDENTIALS, userPassword);

		DirContext context = null;
		try {
			context = new InitialDirContext(env);
			return true; // If binding succeeds, the password is valid
		} catch (AuthenticationException e) {
			logger.info("Invalid credentials for user DN: {}", userDn);
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
}
