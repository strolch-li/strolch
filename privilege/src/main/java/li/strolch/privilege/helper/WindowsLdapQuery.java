package li.strolch.privilege.helper;

import li.strolch.privilege.base.AccessDeniedException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.naming.Context;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.PartialResultException;
import javax.naming.directory.InitialDirContext;
import javax.naming.directory.SearchControls;
import javax.naming.directory.SearchResult;
import java.util.Hashtable;

import static li.strolch.utils.LdapHelper.encodeForLDAP;
import static li.strolch.utils.helper.ExceptionHelper.getExceptionMessage;

public class WindowsLdapQuery implements AutoCloseable {

	protected static final Logger logger = LoggerFactory.getLogger(WindowsLdapQuery.class);

	public static final String LDAP_FILTER_TEMPLATE = "(&%s(%s=%s)%s)";
	public static final String USER_PRINCIPAL_NAME = "userPrincipalName";

	public static final String LDAP_SN = "sn";
	public static final String LDAP_DEPARTMENT = "department";
	public static final String LDAP_MEMBER_OF = "memberOf";
	public static final String LDAP_CN = "CN";
	public static final String LDAP_GIVEN_NAME = "givenName";
	public static final String LDAP_SAM_ACCOUNT_NAME = "sAMAccountName";

	protected final String providerUrl;
	protected final String searchBase;
	protected final String additionalFilter;
	protected final String domain;
	protected final String domainPrefix;

	protected InitialDirContext context;

	public WindowsLdapQuery(String providerUrl, String searchBase, String additionalFilter, String domain,
			String domainPrefix) {
		this.providerUrl = providerUrl;
		this.searchBase = searchBase;
		this.additionalFilter = additionalFilter;
		this.domain = domain;
		this.domainPrefix = domainPrefix;
	}

	private Hashtable<String, String> buildLdapEnv(char[] password, String userPrincipalName) {

		// Set up the environment for creating the initial context
		Hashtable<String, String> env = new Hashtable<>();

		env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
		env.put(Context.PROVIDER_URL, this.providerUrl);

		// Authenticate
		env.put(Context.SECURITY_AUTHENTICATION, "simple");
		env.put(Context.SECURITY_PRINCIPAL, userPrincipalName);
		env.put(Context.SECURITY_CREDENTIALS, new String(password));
		env.put(Context.REFERRAL, "ignore");
		return env;
	}

	protected String getDistinguishedName(String safeUsername) {
		if (this.domain.isEmpty())
			return safeUsername;

		if (!this.domainPrefix.isEmpty() && safeUsername.startsWith(this.domainPrefix)) {
			logger.warn("Trimming domain from given username, to first search in sAMAccountName");
			safeUsername = encodeForLDAP(safeUsername.substring(this.domainPrefix.length()), true);
		}

		return safeUsername + "@" + this.domain;
	}

	public SearchResult searchLdap(String username, char[] password) throws NamingException {

		// escape the user provider username
		String safeUsername = encodeForLDAP(username, true);

		String distinguishedName = getDistinguishedName(safeUsername);
		logger.info("Logging in with {}", distinguishedName);
		this.context = new InitialDirContext(buildLdapEnv(password, distinguishedName));

		SearchControls searchControls = new SearchControls();
		searchControls.setSearchScope(SearchControls.SUBTREE_SCOPE);

		// the first search is using sAMAccountName
		String objectClassFilter = getObjectClassFilter();
		String userAttributeIdentifier1 = getUserAttributeIdentifier1();
		String userAttributeIdentifier2 = getUserAttributeIdentifier2();
		String filter = LDAP_FILTER_TEMPLATE.formatted(objectClassFilter, userAttributeIdentifier1, safeUsername,
				this.additionalFilter);
		logger.info("Searching based on {}, with search base: {}", filter, this.searchBase);
		NamingEnumeration<SearchResult> answer = this.context.search(this.searchBase, filter, searchControls);

		SearchResult searchResult = null;
		while (searchResult == null) {
			try {

				// and if we don't find anything, then we search with userPrincipalName
				if (!answer.hasMore()) {

					logger.warn("No LDAP data retrieved using {}, trying with {}...", userAttributeIdentifier1,
							userAttributeIdentifier2);
					filter = LDAP_FILTER_TEMPLATE.formatted(objectClassFilter, userAttributeIdentifier2,
							distinguishedName, this.additionalFilter);
					logger.info("Searching based on {}, with search base: {}", filter, this.searchBase);
					answer = this.context.search(this.searchBase, filter, searchControls);

					if (!answer.hasMore())
						throw new AccessDeniedException("Could not login user: "
								+ safeUsername
								+ " on Ldap: no LDAP Data, for either sAMAccountName or userPrincipalName searches. Domain used is "
								+ this.domain);
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

	protected String getObjectClassFilter() {
		return "(objectCategory=person)(objectClass=user)";
	}

	protected String getUserAttributeIdentifier1() {
		return LDAP_SAM_ACCOUNT_NAME;
	}

	protected String getUserAttributeIdentifier2() {
		return USER_PRINCIPAL_NAME;
	}

	@Override
	public void close() {
		if (this.context != null) {
			try {
				this.context.close();
			} catch (NamingException e) {
				logger.error("Failed to close context!", e);
			}
		}
	}
}
