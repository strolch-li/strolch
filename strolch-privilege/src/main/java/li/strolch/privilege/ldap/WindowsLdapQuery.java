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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.PartialResultException;
import javax.naming.directory.InitialDirContext;
import javax.naming.directory.SearchResult;

import static li.strolch.utils.LdapHelper.encodeForLDAP;
import static li.strolch.utils.helper.ExceptionHelper.getExceptionMessage;

public class WindowsLdapQuery extends LdapQuery {

	protected static final Logger logger = LoggerFactory.getLogger(WindowsLdapQuery.class);

	public static final String LDAP_FILTER_TEMPLATE = "(&%s(%s=%s)%s)";

	protected final WindowsLdapQueryContext queryContext;
	protected InitialDirContext directoryContext;

	public WindowsLdapQuery(WindowsLdapQueryContext queryContext) {
		this.queryContext = queryContext;
	}

	@Override
	public SearchResult searchLdap(String username, char[] password) throws NamingException {
		// escape the user provider username
		String safeUsername = encodeForLDAP(username, true);

		String loginUsername = this.queryContext.getLoginUsername(username);
		logger.info("Logging in with username {}", loginUsername);
		this.directoryContext = new InitialDirContext(this.queryContext.buildLdapEnv(password, loginUsername));

		String additionalFilter = this.queryContext.getAdditionalFilter();
		String searchBase = this.queryContext.getSearchBase();
		String domain = this.queryContext.getDomain();

		// the first search is using sAMAccountName
		String objectClassFilter = this.queryContext.getObjectClassFilter();
		String userAttributeIdentifier = this.queryContext.getUserAttributeIdentifier();
		String filter = LDAP_FILTER_TEMPLATE.formatted(objectClassFilter, userAttributeIdentifier, safeUsername,
				additionalFilter);
		logger.info("Searching based on {}, with search base: {}", filter, searchBase);
		NamingEnumeration<SearchResult> answer = this.directoryContext.search(searchBase, filter, this.searchControls);

		SearchResult searchResult = null;
		while (searchResult == null) {
			try {

				// and if we don't find anything, then we search with userPrincipalName
				if (!answer.hasMore())
					throw new AccessDeniedException(
							"Could not login user: %s on Ldap: no LDAP Data, for either sAMAccountName or userPrincipalName searches. Domain used is %s".formatted(
									safeUsername, domain));

				searchResult = answer.next();
				if (answer.hasMore())
					throw new AccessDeniedException(
							"Could not login with user: " + safeUsername + " on Ldap: Multiple LDAP Data");

			} catch (PartialResultException e) {
				if (getExceptionMessage(e).contains("Unprocessed Continuation Reference(s)"))
					logger.warn("Ignoring partial result exception, as we are not following referrals!");
				else
					throw e;
			}
		}

		return searchResult;
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
