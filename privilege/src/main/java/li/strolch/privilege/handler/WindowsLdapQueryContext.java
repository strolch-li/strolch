package li.strolch.privilege.handler;

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.helper.GroupsAndRoles;
import li.strolch.privilege.helper.RemoteGroupMappingModel;
import li.strolch.privilege.model.UserState;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserHistory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.naming.Context;
import javax.naming.NamingException;
import javax.naming.directory.Attribute;
import javax.naming.directory.Attributes;
import javax.naming.directory.SearchResult;
import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;
import java.util.*;

import static li.strolch.utils.LdapHelper.encodeForLDAP;
import static li.strolch.utils.helper.StringHelper.isEmpty;
import static li.strolch.utils.helper.StringHelper.trimOrEmpty;

public class WindowsLdapQueryContext {

	private static final Logger logger = LoggerFactory.getLogger(WindowsLdapQueryContext.class);

	public static final String LDAP_SN = "sn";
	public static final String LDAP_DEPARTMENT = "department";
	public static final String LDAP_MEMBER_OF = "memberOf";
	public static final String LDAP_CN = "CN";
	public static final String LDAP_GIVEN_NAME = "givenName";
	public static final String LDAP_SAM_ACCOUNT_NAME = "sAMAccountName";
	public static final String LDAP_USER_PRINCIPAL_NAME = "userPrincipalName";
	public static final String PLATFORM = "platform";
	public static final String PARAM_PROVIDER_URL = "providerUrl";
	public static final String PARAM_SEARCH_BASE = "searchBase";
	public static final String PARAM_ADDITIONAL_FILTER = "additionalFilter";
	public static final String PARAM_DOMAIN = "domain";
	public static final String PARAM_DEFAULT_LOCALE = "defaultLocale";

	protected final String providerUrl;
	protected final String searchBase;
	protected final String additionalFilter;
	protected final String domain;
	protected final String domainPrefix;
	protected final Locale defaultLocale;
	protected final RemoteGroupMappingModel groupMappingModel;

	public WindowsLdapQueryContext(Map<String, String> parameterMap, RemoteGroupMappingModel groupMappingModel) {

		String providerUrl = parameterMap.get(PARAM_PROVIDER_URL);
		logger.info("providerUrl: {}", providerUrl);
		String searchBase = parameterMap.get(PARAM_SEARCH_BASE);
		logger.info("searchBase: {}", searchBase);
		String additionalFilter = trimOrEmpty(parameterMap.get(PARAM_ADDITIONAL_FILTER));
		if (!additionalFilter.isEmpty())
			logger.info("additionalFilter: {}", additionalFilter);
		String domain = trimOrEmpty(parameterMap.get(PARAM_DOMAIN));
		String domainPrefix = null;
		if (!domain.isEmpty()) {
			if (domain.startsWith("@")) {
				logger.warn(
						"Remove the @ symbol from the domain property! Will be added automatically where required.");
				domain = domain.substring(1);
			}

			domainPrefix = domain + '\\';

			logger.info("domain: {}", domain);
			logger.info("domain prefix: {}", domainPrefix);
		}

		Locale defaultLocale = parameterMap.containsKey(PARAM_DEFAULT_LOCALE) ?
				Locale.forLanguageTag(parameterMap.get(PARAM_DEFAULT_LOCALE)) : Locale.getDefault();

		this.providerUrl = providerUrl;
		this.searchBase = searchBase;
		this.additionalFilter = additionalFilter;
		this.domain = domain;
		this.domainPrefix = domainPrefix;
		this.defaultLocale = defaultLocale;
		this.groupMappingModel = groupMappingModel;
	}

	public String getProviderUrl() {
		return this.providerUrl;
	}

	public String getAdditionalFilter() {
		return this.additionalFilter;
	}

	public String getSearchBase() {
		return this.searchBase;
	}

	public String getDomain() {
		return domain;
	}

	public String getDomainPrefix() {
		return domainPrefix;
	}

	public String getObjectClassFilter() {
		return "(objectCategory=person)(objectClass=user)";
	}

	public String getUserAttributeIdentifier1() {
		return LDAP_SAM_ACCOUNT_NAME;
	}

	public String getUserAttributeIdentifier2() {
		return LDAP_USER_PRINCIPAL_NAME;
	}

	public String getDistinguishedName(String safeUsername) {
		if (this.domain.isEmpty())
			return safeUsername;

		if (!this.domainPrefix.isEmpty() && safeUsername.startsWith(this.domainPrefix)) {
			logger.warn("Trimming domain from given username, to first search in sAMAccountName");
			safeUsername = encodeForLDAP(safeUsername.substring(this.domainPrefix.length()), true);
		}

		return safeUsername + "@" + this.domain;
	}

	public String getDepartment(Attributes attrs) throws NamingException {
		return getLdapString(attrs, LDAP_DEPARTMENT);
	}

	public String validateLdapUsername(String username, Attributes attrs) throws NamingException {
		Attribute sAMAccountName = attrs.get(getUserAttributeIdentifier1());
		if (sAMAccountName == null || !username.equalsIgnoreCase(sAMAccountName.get().toString()))
			throw new AccessDeniedException(
					"Could not login with user: " + username + this.domain + " on Ldap: Wrong LDAP Data");

		return sAMAccountName.get().toString();
	}

	public String getLdapString(Attributes attrs, String key) throws NamingException {
		Attribute sn = attrs.get(key);
		return sn == null ? null : sn.get().toString();
	}

	public String getFirstName(String username, Attributes attrs) throws NamingException {
		String value = getLdapString(attrs, LDAP_GIVEN_NAME);
		return isEmpty(value) ? username : value;
	}

	public String getLastName(String username, Attributes attrs) throws NamingException {
		String value = getLdapString(attrs, LDAP_SN);
		return isEmpty(value) ? username : value;
	}

	public Locale getLocale(Attributes attrs) {
		return this.defaultLocale;
	}

	public Set<String> getLdapGroups(Attributes attrs) throws NamingException {
		Set<String> ldapRoles = new HashSet<>();
		Attribute groupMembers = attrs.get(LDAP_MEMBER_OF);
		if (groupMembers == null)
			return ldapRoles;

		for (int i = 0; i < groupMembers.size(); i++) {
			String memberOfLdapString = attrs.get(LDAP_MEMBER_OF).get(i).toString();

			// extract group name from ldap string -> CN=groupname,OU=company,DC=domain,DC=country
			LdapName memberOfName = new LdapName(memberOfLdapString);
			for (Rdn rdn : memberOfName.getRdns()) {
				if (rdn.getType().equalsIgnoreCase(LDAP_CN)) {
					String groupName = rdn.getValue().toString();
					ldapRoles.add(groupName);
					break;
				}
			}
		}

		return ldapRoles;
	}

	public Hashtable<String, String> buildLdapEnv(char[] password, String userPrincipalName) {

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

	public User buildUserFromSearchResult(String username, SearchResult searchResult) throws Exception {
		Attributes attrs = searchResult.getAttributes();

		username = validateLdapUsername(username, attrs);

		String firstName = getFirstName(username, attrs);
		String lastName = getLastName(username, attrs);
		Locale locale = getLocale(attrs);

		// evaluate groups and roles for this user
		Set<String> ldapGroups = this.groupMappingModel.getUserGroupOverride(username, getLdapGroups(attrs));
		GroupsAndRoles groupsAndRoles = this.groupMappingModel.mapRemoteGroupsToStrolch(ldapGroups);

		if (groupsAndRoles.isEmpty()) {
			logger.error("User {} can not login, as no group or role mappings were found.", username);
			logger.info("User {} is member of the following LDAP groups: ", username);
			ldapGroups.forEach(s -> logger.info("  {}", s));
			throw new AccessDeniedException(
					"User " + username + " can not login, as no group or role mappings were found.");
		}

		// first see if we can find the primaryLocation from the department attribute:
		String department = getDepartment(attrs);
		Map<String, String> properties = this.groupMappingModel.buildProperties(department, ldapGroups);

		return new User(username, username, null, firstName, lastName, UserState.REMOTE, groupsAndRoles.groups(),
				groupsAndRoles.roles(), locale, properties, false, UserHistory.EMPTY);
	}
}
