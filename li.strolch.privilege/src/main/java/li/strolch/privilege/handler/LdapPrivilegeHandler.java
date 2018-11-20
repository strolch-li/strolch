package li.strolch.privilege.handler;

import javax.naming.Context;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.directory.*;
import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;
import java.util.*;

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.InvalidCredentialsException;
import li.strolch.privilege.model.UserState;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.policy.PrivilegePolicy;
import li.strolch.utils.dbc.DBC;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LdapPrivilegeHandler extends DefaultPrivilegeHandler {

	protected static final Logger logger = LoggerFactory.getLogger(LdapPrivilegeHandler.class);

	private Map<String, Set<String>> rolesForLdapGroups;
	private String providerUrl;
	private String searchBase;
	private String location;
	private String domain;
	private String adminUsers;

	@Override
	public synchronized void initialize(Map<String, String> parameterMap, EncryptionHandler encryptionHandler,
			PersistenceHandler persistenceHandler, UserChallengeHandler userChallengeHandler,
			SingleSignOnHandler ssoHandler, Map<String, Class<PrivilegePolicy>> policyMap) {

		rolesForLdapGroups = getLdapGroupToRolesMappingFromConfig(parameterMap);

		this.providerUrl = parameterMap.get("providerUrl");
		this.searchBase = parameterMap.get("searchBase");
		this.location = parameterMap.get("location");
		this.domain = parameterMap.get("domain");
		this.adminUsers = parameterMap.get("adminUsers");

		super.initialize(parameterMap, encryptionHandler, persistenceHandler, userChallengeHandler, ssoHandler,
				policyMap);
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

		String memberOfLdapString = "";
		Set<String> strolchRoles = new HashSet<>();

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

			SearchResult sr = (SearchResult) answer.next();
			if (answer.hasMore())
				throw new AccessDeniedException(
						"Could not login with user: " + username + this.domain + " on Ldap: Multiple LDAP Data");

			Attributes attrs = sr.getAttributes();

			Attribute sAMAccountName = attrs.get("sAMAccountName");
			if (sAMAccountName == null || !username.equals(sAMAccountName.get().toString()))
				throw new AccessDeniedException(
						"Could not login with user: " + username + this.domain + " on Ldap: Wrong LDAP Data");

			Attribute givenName = attrs.get("givenName");
			Attribute sn = attrs.get("sn");

			String firstName = givenName == null ? username : givenName.get().toString();
			String lastName = sn == null ? username : sn.get().toString();

			// evaluate roles for this user
			Attribute groupMembers = attrs.get("memberOf");
			logger.info("User " + username + " is member of groups: ");
			if (groupMembers != null) {
				for (int i = 0; i < groupMembers.size(); i++) {

					memberOfLdapString = attrs.get("memberOf").get(i).toString();

					// extract group name from ldap string -> CN=groupname,OU=company,DC=domain,DC=country
					LdapName memberOfName = new LdapName(memberOfLdapString);
					for (Rdn rdn : memberOfName.getRdns()) {
						if (rdn.getType().equalsIgnoreCase("CN")) {
							String groupName = rdn.getValue().toString();
							logger.info(" - " + groupName);
							Set<String> foundStrolchRoles = this.rolesForLdapGroups.get(groupName);
							if (foundStrolchRoles != null)
								strolchRoles.addAll(foundStrolchRoles);
							break;
						}
					}
				}
			}

			Map<String, String> properties = new HashMap<>();

			// this must be changed, because the location param must be taken from the logged in person
			properties.put("location", this.location);

			// see if this is an admin user
			if (this.adminUsers.contains(username))
				strolchRoles = this.rolesForLdapGroups.get("admin");

			User user = new User(username, username, null, null, null, -1, -1, firstName, lastName, UserState.REMOTE,
					strolchRoles, Locale.GERMAN, properties);

			// persist this user
			if (internalUser == null)
				this.persistenceHandler.addUser(user);
			else
				this.persistenceHandler.replaceUser(user);

			if (this.autoPersistOnUserChangesData)
				this.persistenceHandler.persist();

			return user;

		} catch (Exception e) {
			logger.error("Could not login with user: " + username + domain + " on Ldap", e);
			throw new AccessDeniedException("Could not login with user: " + username + domain + " on Ldap", e);
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

	private Map<String, Set<String>> getLdapGroupToRolesMappingFromConfig(Map<String, String> params) {
		Map<String, Set<String>> result = new HashMap<>();

		String rolesForLdapGroups = params.get("rolesForLdapGroups");

		DBC.PRE.assertNotEmpty("No roles mapping for ldap directory groups defined (param: rolesForLdapGroups)",
				rolesForLdapGroups);

		// rolesForLdapGroups = admin=StrolchAdmin,UserPrivileges;user=UserPrivileges
		String[] ldapGroupRoles = rolesForLdapGroups.split(";");

		for (String ldapGroupRole : ldapGroupRoles) {
			// admin=StrolchAdmin,UserPrivileges
			String[] splittedGroupRoles = ldapGroupRole.split("=");
			String ldapGroupName = splittedGroupRoles[0];
			// StrolchAdmin,UserPrivileges
			Set<String> roleNames = new HashSet<>(Arrays.asList(splittedGroupRoles[1].split(",")));

			result.put(ldapGroupName, roleNames);
		}

		return result;
	}

}
