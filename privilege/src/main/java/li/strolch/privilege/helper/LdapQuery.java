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

public class LdapQuery implements AutoCloseable {

	private static final Logger logger = LoggerFactory.getLogger(LdapQuery.class);

	public static final String LDAP_FILTER_TEMPLATE = "(&(objectCategory=person)(objectClass=user)(%s=%s)%s)";
	public static final String USER_PRINCIPAL_NAME = "userPrincipalName";

	public static final String LDAP_SN = "sn";
	public static final String LDAP_DEPARTMENT = "department";
	public static final String LDAP_MEMBER_OF = "memberOf";
	public static final String LDAP_CN = "CN";
	public static final String LDAP_GIVEN_NAME = "givenName";
	public static final String LDAP_SAM_ACCOUNT_NAME = "sAMAccountName";

	private final String providerUrl;
	private final String searchBase;
	private final String additionalFilter;
	private final String domain;
	private final String domainPrefix;

	private InitialDirContext context;

	public LdapQuery(String providerUrl, String searchBase, String additionalFilter, String domain,
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

	public SearchResult searchLdap(String username, char[] password) throws NamingException {

		// escape the user provider username
		String safeUsername = encodeForLDAP(username, true);

		// sanitize username
		String userPrincipalName;
		if (this.domain.isEmpty()) {
			userPrincipalName = safeUsername;
		} else {
			if (!this.domainPrefix.isEmpty() && username.startsWith(this.domainPrefix)) {
				logger.warn("Trimming domain from given username, to first search in sAMAccountName");
				safeUsername = encodeForLDAP(username.substring(this.domainPrefix.length()), true);
			}
			userPrincipalName = safeUsername + "@" + this.domain;
		}

		context = new InitialDirContext(buildLdapEnv(password, userPrincipalName));

		SearchControls searchControls = new SearchControls();
		searchControls.setSearchScope(SearchControls.SUBTREE_SCOPE);

		// the first search is using sAMAccountName
		NamingEnumeration<SearchResult> answer = context.search(this.searchBase,
				LDAP_FILTER_TEMPLATE.formatted(LDAP_SAM_ACCOUNT_NAME, safeUsername, this.additionalFilter),
				searchControls);

		SearchResult searchResult = null;
		while (searchResult == null) {
			try {

				// and if we don't find anything, then we search with userPrincipalName
				if (!answer.hasMore()) {

					logger.warn("No LDAP data retrieved using {}, trying with {}...", LDAP_SAM_ACCOUNT_NAME,
							USER_PRINCIPAL_NAME);
					answer = context.search(this.searchBase,
							LDAP_FILTER_TEMPLATE.formatted(USER_PRINCIPAL_NAME, userPrincipalName,
									this.additionalFilter), searchControls);

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
