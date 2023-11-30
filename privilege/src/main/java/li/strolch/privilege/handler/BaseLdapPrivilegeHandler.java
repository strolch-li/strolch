package li.strolch.privilege.handler;

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.InvalidCredentialsException;
import li.strolch.privilege.model.UserState;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserHistory;
import li.strolch.privilege.policy.PrivilegePolicy;
import li.strolch.utils.helper.ExceptionHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.naming.*;
import javax.naming.directory.*;
import java.util.Hashtable;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ScheduledExecutorService;

import static li.strolch.utils.LdapHelper.encodeForLDAP;
import static li.strolch.utils.helper.StringHelper.trimOrEmpty;

public abstract class BaseLdapPrivilegeHandler extends DefaultPrivilegeHandler {

	protected static final Logger logger = LoggerFactory.getLogger(BaseLdapPrivilegeHandler.class);
	public static final String LDAP_FILTER_TEMPLATE = "(&(objectCategory=person)(objectClass=user)(%s=%s)%s)";
	public static final String SAM_ACCOUNT_NAME = "sAMAccountName";
	public static final String USER_PRINCIPAL_NAME = "userPrincipalName";

	private String providerUrl;
	private String searchBase;
	private String additionalFilter;
	private String domain;
	private String domainPrefix;

	@Override
	public void initialize(ScheduledExecutorService executorService, Map<String, String> parameterMap,
			EncryptionHandler encryptionHandler, PasswordStrengthHandler passwordStrengthHandler,
			PersistenceHandler persistenceHandler, UserChallengeHandler userChallengeHandler,
			SingleSignOnHandler ssoHandler, Map<String, Class<PrivilegePolicy>> policyMap) {

		super.initialize(executorService, parameterMap, encryptionHandler, passwordStrengthHandler, persistenceHandler,
				userChallengeHandler, ssoHandler, policyMap);

		this.providerUrl = parameterMap.get("providerUrl");
		logger.info("providerUrl: " + this.providerUrl);
		this.searchBase = parameterMap.get("searchBase");
		logger.info("searchBase: " + this.searchBase);
		this.additionalFilter = trimOrEmpty(parameterMap.get("additionalFilter"));
		if (!this.additionalFilter.isEmpty())
			logger.info("additionalFilter: " + this.additionalFilter);
		this.domain = trimOrEmpty(parameterMap.get("domain"));
		if (!this.domain.isEmpty()) {
			if (this.domain.startsWith("@")) {
				logger.warn(
						"Remove the @ symbol from the domain property! Will be added automatically where required.");
				this.domain = this.domain.substring(1);
			}
			this.domainPrefix = this.domain + '\\';

			logger.info("domain: " + this.domain);
			logger.info("domain prefix: " + this.domainPrefix);
		}
	}

	@Override
	protected User checkCredentialsAndUserState(String username, char[] password) throws AccessDeniedException {
		// escape the user provider username
		String safeUsername = encodeForLDAP(username, true);

		// first see if this is a local user
		User internalUser = this.persistenceHandler.getUser(safeUsername);
		if (internalUser != null && internalUser.getUserState() != UserState.REMOTE)
			return super.checkCredentialsAndUserState(safeUsername, password);

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

		logger.info("User {} tries to login on ldap {}", safeUsername, this.providerUrl);

		// Create the initial context
		DirContext ctx = null;
		try {
			ctx = new InitialDirContext(buildLdapEnv(password, userPrincipalName));
			SearchResult searchResult = searchLdap(safeUsername, ctx, userPrincipalName);
			User user = buildUserFromSearchResult(safeUsername, searchResult);

			// persist this user
			if (internalUser == null)
				this.persistenceHandler.addUser(user);
			else
				this.persistenceHandler.replaceUser(user);

			if (this.autoPersistOnUserChangesData)
				persistModelAsync();

			return user;

		} catch (AccessDeniedException e) {
			throw e;
		} catch (AuthenticationException e) {
			logger.error("Could not login with user: " + safeUsername + " on Ldap", e);
			throw new InvalidCredentialsException("Could not login with user: " + safeUsername + " on Ldap", e);
		} catch (Exception e) {
			logger.error("Could not login with user: " + safeUsername + " on Ldap", e);
			throw new AccessDeniedException("Could not login with user: " + safeUsername + " on Ldap", e);
		} finally {
			if (ctx != null) {
				try {
					ctx.close();
				} catch (NamingException e) {
					logger.error("Failed to close DirContext", e);
				}
			}
		}
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

	private SearchResult searchLdap(String safeUsername, DirContext ctx, String userPrincipalName)
			throws NamingException {
		SearchControls searchControls = new SearchControls();
		searchControls.setSearchScope(SearchControls.SUBTREE_SCOPE);

		// the first search is using sAMAccountName
		NamingEnumeration<SearchResult> answer = ctx.search(this.searchBase,
				LDAP_FILTER_TEMPLATE.formatted(SAM_ACCOUNT_NAME, safeUsername, this.additionalFilter), searchControls);

		SearchResult searchResult = null;
		while (searchResult == null) {
			try {

				// and if we don't find anything, then we search with userPrincipalName
				if (!answer.hasMore()) {

					logger.warn("No LDAP data retrieved using "
							+ SAM_ACCOUNT_NAME
							+ ", trying with "
							+ USER_PRINCIPAL_NAME
							+ "...");
					answer = ctx.search(this.searchBase,
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
				if (ExceptionHelper.getExceptionMessage(e).contains("Unprocessed Continuation Reference(s)"))
					logger.warn("Ignoring partial result exception, as we are not following referrals!");
				else
					throw e;
			}
		}

		return searchResult;
	}

	protected User buildUserFromSearchResult(String username, SearchResult sr) throws Exception {
		Attributes attrs = sr.getAttributes();

		username = validateLdapUsername(username, attrs);

		String firstName = getFirstName(username, attrs);
		String lastName = getLastName(username, attrs);
		Locale locale = getLocale(attrs);

		// evaluate roles for this user
		Set<String> ldapGroups = getLdapGroups(username, attrs);
		logger.info("User " + username + " is member of the following LDAP groups: ");
		ldapGroups.forEach(s -> logger.info("- " + s));
		Set<String> strolchGroups = mapToStrolchGroups(username, ldapGroups);
		Set<String> strolchRoles = mapToStrolchRoles(username, ldapGroups);

		Map<String, String> properties = buildProperties(username, attrs, ldapGroups, strolchRoles);

		return new User(username, username, null, firstName, lastName, UserState.REMOTE, strolchGroups, strolchRoles,
				locale, properties, false, UserHistory.EMPTY);
	}

	protected abstract Map<String, String> buildProperties(String username, Attributes attrs, Set<String> ldapGroups,
			Set<String> strolchRoles) throws Exception;

	protected String validateLdapUsername(String username, Attributes attrs) throws NamingException {
		Attribute sAMAccountName = attrs.get(SAM_ACCOUNT_NAME);
		if (sAMAccountName == null || !username.equalsIgnoreCase(sAMAccountName.get().toString()))
			throw new AccessDeniedException(
					"Could not login with user: " + username + this.domain + " on Ldap: Wrong LDAP Data");

		return sAMAccountName.get().toString();
	}

	protected String getLdapString(Attributes attrs, String key) throws NamingException {
		Attribute sn = attrs.get(key);
		return sn == null ? null : sn.get().toString();
	}

	protected abstract String getFirstName(String username, Attributes attrs) throws NamingException;

	protected abstract String getLastName(String username, Attributes attrs) throws NamingException;

	protected abstract Locale getLocale(Attributes attrs);

	protected abstract Set<String> getLdapGroups(String username, Attributes attrs) throws NamingException;

	protected abstract Set<String> mapToStrolchGroups(String username, Set<String> ldapGroups);

	protected abstract Set<String> mapToStrolchRoles(String username, Set<String> ldapGroups);
}
