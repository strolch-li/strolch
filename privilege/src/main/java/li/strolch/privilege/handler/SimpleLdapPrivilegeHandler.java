package li.strolch.privilege.handler;

import static li.strolch.privilege.base.PrivilegeConstants.*;
import static li.strolch.utils.helper.StringHelper.isEmpty;

import javax.naming.NamingException;
import javax.naming.directory.Attributes;
import java.util.*;
import java.util.concurrent.ScheduledExecutorService;

import li.strolch.privilege.helper.LdapHelper;
import li.strolch.privilege.policy.PrivilegePolicy;
import li.strolch.utils.dbc.DBC;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SimpleLdapPrivilegeHandler extends BaseLdapPrivilegeHandler {

	protected static final Logger logger = LoggerFactory.getLogger(SimpleLdapPrivilegeHandler.class);

	private Locale defaultLocale;
	private String adminUsers;
	private Map<String, Set<String>> rolesForLdapGroups;
	private String organisation;
	private String location;
	private String realm;

	@Override
	public synchronized void initialize(ScheduledExecutorService executorService, Map<String, String> parameterMap,
			EncryptionHandler encryptionHandler, PasswordStrengthHandler passwordStrengthHandler,
			PersistenceHandler persistenceHandler, UserChallengeHandler userChallengeHandler,
			SingleSignOnHandler ssoHandler, Map<String, Class<PrivilegePolicy>> policyMap) {

		super.initialize(executorService, parameterMap, encryptionHandler, passwordStrengthHandler, persistenceHandler,
				userChallengeHandler, ssoHandler, policyMap);

		this.organisation = parameterMap.getOrDefault(ORGANISATION, "");
		this.location = parameterMap.getOrDefault(LOCATION, "");
		this.realm = parameterMap.getOrDefault(REALM, "");

		this.defaultLocale = parameterMap.containsKey("defaultLocale") ?
				Locale.forLanguageTag(parameterMap.get("defaultLocale")) :
				Locale.getDefault();

		this.adminUsers = parameterMap.get("adminUsers");
		this.rolesForLdapGroups = getLdapGroupToRolesMappingFromConfig(parameterMap);
	}

	@Override
	protected String getFirstName(String username, Attributes attrs) throws NamingException {
		String value = getLdapString(attrs, "givenName");
		return isEmpty(value) ? username : value;
	}

	@Override
	protected String getLastName(String username, Attributes attrs) throws NamingException {
		String value = getLdapString(attrs, "sn");
		return isEmpty(value) ? username : value;
	}

	@Override
	protected Map<String, String> buildProperties(String username, Attributes attrs, Set<String> ldapGroups,
			Set<String> strolchRoles) {

		Map<String, String> properties = new HashMap<>();
		properties.put(ORGANISATION, this.organisation);
		properties.put(LOCATION, this.location);
		properties.put(REALM, this.realm);
		return properties;
	}

	@Override
	protected Locale getLocale(Attributes attrs) {
		return this.defaultLocale;
	}

	@Override
	protected Set<String> getLdapGroups(String username, Attributes attrs) throws NamingException {
		Set<String> ldapGroups = LdapHelper.getLdapGroups(attrs);
		logger.info("User " + username + " has LDAP Groups: ");
		ldapGroups.forEach(s -> logger.info("- " + s));
		return ldapGroups;
	}

	@Override
	protected Set<String> mapToStrolchRoles(String username, Set<String> ldapGroups) {
		Set<String> strolchRoles = new HashSet<>();
		for (String ldapRole : ldapGroups) {
			Set<String> foundStrolchRoles = this.rolesForLdapGroups.get(ldapRole);
			if (foundStrolchRoles != null)
				strolchRoles.addAll(foundStrolchRoles);
		}

		// see if this is an admin user
		if (this.adminUsers.contains(username))
			strolchRoles = this.rolesForLdapGroups.get("admin");

		return strolchRoles;
	}

	private Map<String, Set<String>> getLdapGroupToRolesMappingFromConfig(Map<String, String> params) {

		String rolesForLdapGroups = params.get("rolesForLdapGroups");
		DBC.PRE.assertNotEmpty("No roles mapping for ldap directory groups defined (param: rolesForLdapGroups)",
				rolesForLdapGroups);

		// rolesForLdapGroups = admin=StrolchAdmin,UserPrivileges;user=UserPrivileges

		String[] ldapGroupRoles = rolesForLdapGroups.split(";");

		Map<String, Set<String>> result = new HashMap<>();
		for (String ldapGroupRole : ldapGroupRoles) {
			ldapGroupRole = ldapGroupRole.trim();

			String[] splitGroupRoles = ldapGroupRole.split("=");
			String ldapGroupName = splitGroupRoles[0];
			String[] strolchRoles = splitGroupRoles[1].split(",");
			Set<String> roleNames = new HashSet<>();
			for (String strolchRole : strolchRoles) {
				roleNames.add(strolchRole.trim());
			}

			result.put(ldapGroupName, roleNames);
		}

		return result;
	}
}
