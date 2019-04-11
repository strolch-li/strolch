package li.strolch.privilege.handler;

import javax.naming.Context;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.directory.*;
import java.util.Hashtable;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.InvalidCredentialsException;
import li.strolch.privilege.model.UserState;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.policy.PrivilegePolicy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class BaseLdapPrivilegeHandler extends DefaultPrivilegeHandler {

	protected static final Logger logger = LoggerFactory.getLogger(BaseLdapPrivilegeHandler.class);

	private String providerUrl;
	private String searchBase;
	private String domain;

	@Override
	public synchronized void initialize(Map<String, String> parameterMap, EncryptionHandler encryptionHandler,
			PersistenceHandler persistenceHandler, UserChallengeHandler userChallengeHandler,
			SingleSignOnHandler ssoHandler, Map<String, Class<PrivilegePolicy>> policyMap) {

		super.initialize(parameterMap, encryptionHandler, persistenceHandler, userChallengeHandler, ssoHandler,
				policyMap);

		this.providerUrl = parameterMap.get("providerUrl");
		this.searchBase = parameterMap.get("searchBase");
		this.domain = parameterMap.get("domain");

	}

	@Override
	protected synchronized User checkCredentialsAndUserState(String username, char[] password)
			throws InvalidCredentialsException, AccessDeniedException {

		// first see if this is a local user
		User internalUser = this.persistenceHandler.getUser(username);
		if (internalUser != null && internalUser.getUserState() != UserState.REMOTE)
			return super.checkCredentialsAndUserState(username, password);

		// Set up the environment for creating the initial context
		Hashtable<String, String> env = new Hashtable<>();

		env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
		env.put(Context.PROVIDER_URL, this.providerUrl);

		// Authenticate 
		env.put(Context.SECURITY_AUTHENTICATION, "simple");
		env.put(Context.SECURITY_PRINCIPAL, username + this.domain);
		env.put(Context.SECURITY_CREDENTIALS, new String(password));

		logger.info("User {} tries to login on ldap", username + this.domain);

		// Create the initial context
		DirContext ctx = null;
		try {
			ctx = new InitialDirContext(env);

			//Create the search controls        
			SearchControls searchCtls = new SearchControls();

			//Specify the search scope
			searchCtls.setSearchScope(SearchControls.SUBTREE_SCOPE);

			String searchFilter =
					"(&(objectCategory=person)(objectClass=user)(userPrincipalName=" + username + this.domain + "))";

			// Search for objects using the filter
			NamingEnumeration<SearchResult> answer = ctx.search(this.searchBase, searchFilter, searchCtls);

			if (!answer.hasMore()) {

				logger.warn("No LDAP data retrieved using userPrincipalName, trying with sAMAccountName...");
				searchFilter = "(&(objectCategory=person)(objectClass=user)(sAMAccountName=" + username + "))";
				answer = ctx.search(this.searchBase, searchFilter, searchCtls);

				if (!answer.hasMore())
					throw new AccessDeniedException("Could not login with user: " + username + this.domain
							+ " on Ldap: no LDAP Data, for either userPrincipalName or sAMAccountName");
			}

			SearchResult searchResult = answer.next();
			if (answer.hasMore())
				throw new AccessDeniedException(
						"Could not login with user: " + username + this.domain + " on Ldap: Multiple LDAP Data");

			User user = buildUserFromSearchResult(username, searchResult);

			// persist this user
			if (internalUser == null)
				this.persistenceHandler.addUser(user);
			else
				this.persistenceHandler.replaceUser(user);

			if (this.autoPersistOnUserChangesData)
				this.persistenceHandler.persist();

			return user;

		} catch (Exception e) {
			logger.error("Could not login with user: " + username + this.domain + " on Ldap", e);
			throw new AccessDeniedException("Could not login with user: " + username + this.domain + " on Ldap", e);
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

	protected User buildUserFromSearchResult(String username, SearchResult sr) throws NamingException {
		Attributes attrs = sr.getAttributes();

		validateLdapUsername(username, attrs);

		String firstName = getFirstName(username, attrs);
		String lastName = getLastName(username, attrs);
		Locale locale = getLocale(attrs);

		// evaluate roles for this user
		Set<String> ldapGroups = getLdapGroups(username, attrs);
		logger.info("User " + username + " is member of the following LDAP groups: ");
		ldapGroups.forEach(s -> logger.info("- " + s));
		Set<String> strolchRoles = mapToStrolchRoles(username, ldapGroups);

		Map<String, String> properties = buildProperties(username, attrs, ldapGroups, strolchRoles);

		return new User(username, username, null, null, null, -1, -1, firstName, lastName, UserState.REMOTE,
				strolchRoles, locale, properties);
	}

	protected abstract Map<String, String> buildProperties(String username, Attributes attrs, Set<String> ldapGroups,
			Set<String> strolchRoles) throws NamingException;

	protected void validateLdapUsername(String username, Attributes attrs) throws NamingException {
		Attribute sAMAccountName = attrs.get("sAMAccountName");
		if (sAMAccountName == null || !username.equals(sAMAccountName.get().toString()))
			throw new AccessDeniedException(
					"Could not login with user: " + username + this.domain + " on Ldap: Wrong LDAP Data");
	}

	protected String getLdapString(Attributes attrs, String key) throws NamingException {
		Attribute sn = attrs.get(key);
		return sn == null ? null : sn.get().toString();
	}

	protected abstract String getFirstName(String username, Attributes attrs) throws NamingException;

	protected abstract String getLastName(String username, Attributes attrs) throws NamingException;

	protected abstract Locale getLocale(Attributes attrs) throws NamingException;

	protected abstract Set<String> getLdapGroups(String username, Attributes attrs) throws NamingException;

	protected abstract Set<String> mapToStrolchRoles(String username, Set<String> ldapGroups);
}
