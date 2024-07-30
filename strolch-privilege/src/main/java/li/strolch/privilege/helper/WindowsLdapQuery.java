package li.strolch.privilege.helper;

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.handler.WindowsLdapQueryContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.PartialResultException;
import javax.naming.directory.InitialDirContext;
import javax.naming.directory.SearchControls;
import javax.naming.directory.SearchResult;

import static li.strolch.utils.LdapHelper.encodeForLDAP;
import static li.strolch.utils.helper.ExceptionHelper.getExceptionMessage;

public class WindowsLdapQuery implements AutoCloseable {

	protected static final Logger logger = LoggerFactory.getLogger(WindowsLdapQuery.class);

	public static final String LDAP_FILTER_TEMPLATE = "(&%s(%s=%s)%s)";

	protected final WindowsLdapQueryContext queryContext;
	protected InitialDirContext directoryContext;
	protected SearchControls searchControls;

	public WindowsLdapQuery(WindowsLdapQueryContext queryContext) {
		this.queryContext = queryContext;

		this.searchControls = new SearchControls();
		this.searchControls.setSearchScope(SearchControls.SUBTREE_SCOPE);
		this.searchControls.setReturningAttributes(new String[]{"*", "+"});
	}

	public SearchResult searchLdap(String username, char[] password) throws NamingException {

		// escape the user provider username
		String safeUsername = encodeForLDAP(username, true);

		String distinguishedName = this.queryContext.getDistinguishedName(safeUsername);
		logger.info("Logging in with {}", distinguishedName);
		this.directoryContext = new InitialDirContext(this.queryContext.buildLdapEnv(password, distinguishedName));

		String additionalFilter = this.queryContext.getAdditionalFilter();
		String searchBase = this.queryContext.getSearchBase();
		String domain = this.queryContext.getDomain();

		// the first search is using sAMAccountName
		String objectClassFilter = this.queryContext.getObjectClassFilter();
		String userAttributeIdentifier1 = this.queryContext.getUserAttributeIdentifier1();
		String userAttributeIdentifier2 = this.queryContext.getUserAttributeIdentifier2();
		String filter = LDAP_FILTER_TEMPLATE.formatted(objectClassFilter, userAttributeIdentifier1, safeUsername,
				additionalFilter);
		logger.info("Searching based on {}, with search base: {}", filter, searchBase);
		NamingEnumeration<SearchResult> answer = this.directoryContext.search(searchBase, filter, this.searchControls);

		SearchResult searchResult = null;
		while (searchResult == null) {
			try {

				// and if we don't find anything, then we search with userPrincipalName
				if (!answer.hasMore()) {

					logger.warn("No LDAP data retrieved using {}, trying with {}...", userAttributeIdentifier1,
							userAttributeIdentifier2);
					filter = LDAP_FILTER_TEMPLATE.formatted(objectClassFilter, userAttributeIdentifier2,
							distinguishedName, additionalFilter);
					logger.info("Searching based on {}, with search base: {}", filter, searchBase);
					answer = this.directoryContext.search(searchBase, filter, this.searchControls);

					if (!answer.hasMore())
						throw new AccessDeniedException("Could not login user: "
								+ safeUsername
								+ " on Ldap: no LDAP Data, for either sAMAccountName or userPrincipalName searches. Domain used is "
								+ domain);
				}

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
