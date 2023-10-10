package li.strolch.privilege.handler;

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.model.UserState;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserHistory;
import li.strolch.privilege.policy.PrivilegePolicy;
import li.strolch.utils.helper.ExceptionHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.naming.Context;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.PartialResultException;
import javax.naming.directory.*;
import java.util.Hashtable;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ScheduledExecutorService;

import static li.strolch.utils.helper.StringHelper.*;

public abstract class BaseLdapPrivilegeHandler extends DefaultPrivilegeHandler {

	protected static final Logger logger = LoggerFactory.getLogger(BaseLdapPrivilegeHandler.class);

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
		if (isNotEmpty(this.additionalFilter))
			logger.info("additionalFilter: " + this.additionalFilter);
		this.domain = parameterMap.get("domain");
		if (isNotEmpty(this.domain)) {
			if (this.domain.startsWith("@")) {
				logger.warn(
						"Remove the @ symbol from the domain property! Will be added automatically where required.");
				this.domain = this.domain.substring(1);
			}

			logger.info("domain: " + this.domain);
			this.domainPrefix = this.domain + "\\";
		}
	}

	@Override
	protected User checkCredentialsAndUserState(String username, char[] password) throws AccessDeniedException {

		// first see if this is a local user
		User internalUser = this.persistenceHandler.getUser(username);
		if (internalUser != null && internalUser.getUserState() != UserState.REMOTE)
			return super.checkCredentialsAndUserState(username, password);

		String userPrincipalName;
		if (isEmpty(this.domain)) {
			userPrincipalName = username;
		} else {
			if (!this.domainPrefix.isEmpty() && username.startsWith(this.domainPrefix)) {
				logger.warn("Trimming domain from given username, to first search in sAMAccountName");
				username = username.substring(this.domainPrefix.length());
			}
			userPrincipalName = username + "@" + this.domain;
		}

		// Set up the environment for creating the initial context
		Hashtable<String, String> env = new Hashtable<>();

		env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
		env.put(Context.PROVIDER_URL, this.providerUrl);

		// Authenticate 
		env.put(Context.SECURITY_AUTHENTICATION, "simple");
		env.put(Context.SECURITY_PRINCIPAL, userPrincipalName);
		env.put(Context.SECURITY_CREDENTIALS, new String(password));
		env.put(Context.REFERRAL, "ignore");

		logger.info("User {} tries to login on ldap {}", username, this.providerUrl);

		// Create the initial context
		DirContext ctx = null;
		try {
			ctx = new InitialDirContext(env);

			//Create the search controls        
			SearchControls searchControls = new SearchControls();

			//Specify the search scope
			searchControls.setSearchScope(SearchControls.SUBTREE_SCOPE);

			String searchFilter = "(&(objectCategory=person)(objectClass=user)(sAMAccountName=%s)%s)".formatted(
					username, this.additionalFilter);

			// Search for objects using the filter
			NamingEnumeration<SearchResult> answer = ctx.search(this.searchBase, searchFilter, searchControls);

			SearchResult searchResult = null;
			while (searchResult == null) {
				try {
					if (!answer.hasMore()) {

						logger.warn("No LDAP data retrieved using sAMAccountName, trying with userPrincipalName...");
						searchFilter = "(&(objectCategory=person)(objectClass=user)(userPrincipalName=%s)%s)".formatted(
								userPrincipalName, this.additionalFilter);
						answer = ctx.search(this.searchBase, searchFilter, searchControls);

						if (!answer.hasMore())
							throw new AccessDeniedException("Could not login user: " + username +
									" on Ldap: no LDAP Data, for either sAMAccountName or userPrincipalName searches. Domain used is " +
									this.domain);
					}

					searchResult = answer.next();
					if (answer.hasMore())
						throw new AccessDeniedException(
								"Could not login with user: " + username + " on Ldap: Multiple LDAP Data");
				} catch (PartialResultException e) {
					if (ExceptionHelper.getExceptionMessage(e).contains("Unprocessed Continuation Reference(s)"))
						logger.warn("Ignoring partial result exception, as we are not following referrals!");
					else
						throw e;
				}
			}

			User user = buildUserFromSearchResult(username, searchResult);

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
		} catch (Exception e) {
			logger.error("Could not login with user: " + username + " on Ldap", e);
			throw new AccessDeniedException("Could not login with user: " + username + " on Ldap", e);
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
		Attribute sAMAccountName = attrs.get("sAMAccountName");
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
