package li.strolch.privilege.handler;

import javax.naming.Context;
import javax.naming.NamingEnumeration;
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
		if (this.persistenceHandler.getUser(username) != null)
			return super.checkCredentialsAndUserState(username, password);

		// Set up the environment for creating the initial context
		Hashtable<String, String> env = new Hashtable<>();

		env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
		env.put(Context.PROVIDER_URL, providerUrl);

		// Authenticate 
		env.put(Context.SECURITY_AUTHENTICATION, "simple");
		env.put(Context.SECURITY_PRINCIPAL, username + domain);
		env.put(Context.SECURITY_CREDENTIALS, new String(password));

		logger.info("User {} tries to login on ldap", username + domain);

		String memberOfLdapString = "";
		Set<String> strolchRoles = new HashSet<>();

		// Create the initial context
		try {
			DirContext ctx = new InitialDirContext(env);

			//Create the search controls        
			SearchControls searchCtls = new SearchControls();

			//Specify the search scope
			searchCtls.setSearchScope(SearchControls.SUBTREE_SCOPE);

			String searchFilter =
					"(&(objectCategory=person)(objectClass=user)(userPrincipalName=" + username + domain + "))";

			// Search for objects using the filter
			NamingEnumeration<SearchResult> answer = ctx.search(searchBase, searchFilter, searchCtls);

			//Loop through the search results
			while (answer.hasMoreElements()) {
				SearchResult sr = (SearchResult) answer.next();

				Attributes attrs = sr.getAttributes();
				Attribute groupMembers = attrs.get("memberOf");

				if (groupMembers != null) {
					for (int i = 0; i < groupMembers.size(); i++) {

						memberOfLdapString = attrs.get("memberOf").get(i).toString();

						// extract group name from ldap string -> CN=groupname,OU=company,DC=domain,DC=country
						LdapName memberOfName = new LdapName(memberOfLdapString);
						for (Rdn rdn : memberOfName.getRdns()) {
							if (rdn.getType().equalsIgnoreCase("CN")) {
								String groupName = rdn.getValue().toString();
								Set<String> foundStrolchRoles = rolesForLdapGroups.get(groupName);
								if (foundStrolchRoles != null)
									strolchRoles.addAll(foundStrolchRoles);
								break;
							}
						}

						logger.info("User " + username + " is member of groups: " + memberOfLdapString);
					}
				}
			}

			ctx.close();
		} catch (Exception e) {
			logger.error("Could not login with user: " + username + domain + " on Ldap", e);
			throw new AccessDeniedException("Could not login with user: " + username + domain + " on Ldap", e);
		}

		Map<String, String> properties = new HashMap<>();

		// this must be changed, because the location param must be taken from the logged in person
		properties.put("location", location);

		if (adminUsers.contains(username)) {
			strolchRoles = rolesForLdapGroups.get("admin");
		}

		return new User(username, username, null, null, null, -1, -1, username, username, UserState.ENABLED,
				strolchRoles, Locale.GERMAN, properties);

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
