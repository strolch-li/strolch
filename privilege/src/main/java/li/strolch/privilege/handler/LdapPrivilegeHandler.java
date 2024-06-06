package li.strolch.privilege.handler;

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.InvalidCredentialsException;
import li.strolch.privilege.helper.GroupsAndRoles;
import li.strolch.privilege.helper.WindowsLdapQuery;
import li.strolch.privilege.helper.RemoteGroupMappingModel;
import li.strolch.privilege.model.UserState;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserHistory;
import li.strolch.privilege.policy.PrivilegePolicy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.naming.AuthenticationException;
import javax.naming.NamingException;
import javax.naming.directory.Attribute;
import javax.naming.directory.Attributes;
import javax.naming.directory.SearchResult;
import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;
import java.util.*;
import java.util.concurrent.ScheduledExecutorService;

import static li.strolch.privilege.base.PrivilegeConstants.REALM;
import static li.strolch.privilege.helper.WindowsLdapQuery.*;
import static li.strolch.privilege.helper.XmlConstants.PARAM_BASE_PATH;
import static li.strolch.privilege.helper.XmlConstants.PARAM_CONFIG_FILE;
import static li.strolch.utils.helper.StringHelper.isEmpty;
import static li.strolch.utils.helper.StringHelper.trimOrEmpty;

public class LdapPrivilegeHandler extends DefaultPrivilegeHandler {

	protected static final Logger logger = LoggerFactory.getLogger(LdapPrivilegeHandler.class);

	private String providerUrl;
	private String searchBase;
	private String additionalFilter;
	private String domain;
	private String domainPrefix;

	private Locale defaultLocale;

	private RemoteGroupMappingModel groupMappingModel;

	@Override
	public void initialize(ScheduledExecutorService executorService, Map<String, String> parameterMap,
			EncryptionHandler encryptionHandler, PasswordStrengthHandler passwordStrengthHandler,
			PersistenceHandler persistenceHandler, UserChallengeHandler userChallengeHandler,
			SingleSignOnHandler ssoHandler, Map<String, Class<PrivilegePolicy>> policyMap) {

		super.initialize(executorService, parameterMap, encryptionHandler, passwordStrengthHandler, persistenceHandler,
				userChallengeHandler, ssoHandler, policyMap);

		this.providerUrl = parameterMap.get("providerUrl");
		logger.info("providerUrl: {}", this.providerUrl);
		this.searchBase = parameterMap.get("searchBase");
		logger.info("searchBase: {}", this.searchBase);
		this.additionalFilter = trimOrEmpty(parameterMap.get("additionalFilter"));
		if (!this.additionalFilter.isEmpty())
			logger.info("additionalFilter: {}", this.additionalFilter);
		this.domain = trimOrEmpty(parameterMap.get("domain"));
		if (!this.domain.isEmpty()) {
			if (this.domain.startsWith("@")) {
				logger.warn(
						"Remove the @ symbol from the domain property! Will be added automatically where required.");
				this.domain = this.domain.substring(1);
			}
			this.domainPrefix = this.domain + '\\';

			logger.info("domain: {}", this.domain);
			logger.info("domain prefix: {}", this.domainPrefix);
		}

		this.defaultLocale = parameterMap.containsKey("defaultLocale") ?
				Locale.forLanguageTag(parameterMap.get("defaultLocale")) : Locale.getDefault();

		String realm = parameterMap.get(REALM);
		String basePath = parameterMap.get(PARAM_BASE_PATH);
		String configFileS = parameterMap.get(PARAM_CONFIG_FILE);
		this.groupMappingModel = new RemoteGroupMappingModel(persistenceHandler);
		this.groupMappingModel.loadJsonConfig(realm, basePath, configFileS);
	}

	@Override
	protected User checkCredentialsAndUserState(String username, char[] password) throws AccessDeniedException {

		// first see if this is a local user
		User internalUser = this.persistenceHandler.getUser(username);
		if (internalUser != null && internalUser.getUserState() != UserState.REMOTE)
			return super.checkCredentialsAndUserState(username, password);

		logger.info("User {} tries to login on ldap {}", username, this.providerUrl);

		// Perform LDAP query
		SearchResult searchResult;
		try (WindowsLdapQuery query = new WindowsLdapQuery(this.providerUrl, this.searchBase, this.additionalFilter, this.domain,
				this.domainPrefix)) {
			searchResult = query.searchLdap(username, password);
		} catch (NamingException e) {
			logger.error("Could not login with user: {} on Ldap", username, e);
			throw new AccessDeniedException("Could not login with user: " + username + " on Ldap", e);
		} finally {
			Arrays.fill(password, '\0');
		}

		// build user
		try {
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
		} catch (AuthenticationException e) {
			logger.error("Could not login with user: {} on Ldap", username, e);
			throw new InvalidCredentialsException("Could not login with user: " + username + " on Ldap", e);
		} catch (Exception e) {
			logger.error("Could not login with user: {} on Ldap", username, e);
			throw new AccessDeniedException("Could not login with user: " + username + " on Ldap", e);
		}
	}

	protected User buildUserFromSearchResult(String username, SearchResult sr) throws Exception {
		Attributes attrs = sr.getAttributes();

		username = validateLdapUsername(username, attrs);

		String firstName = getFirstName(username, attrs);
		String lastName = getLastName(username, attrs);
		Locale locale = getLocale(attrs);

		// evaluate groups and roles for this user
		Set<String> ldapGroups = getLdapGroups(attrs);
		GroupsAndRoles groupsAndRoles = this.groupMappingModel.mapRemoteGroupsToStrolch(username, ldapGroups);

		if (groupsAndRoles.isEmpty()) {
			logger.error("User {} can not login, as no group or role mappings were found.", username);
			logger.info("User {} is member of the following LDAP groups: ", username);
			ldapGroups.forEach(s -> logger.info("- {}", s));
			throw new AccessDeniedException(
					"User " + username + " can not login, as no group or role mappings were found.");
		}

		// first see if we can find the primaryLocation from the department attribute:
		String department = getLdapString(attrs, LDAP_DEPARTMENT);
		Map<String, String> properties = this.groupMappingModel.buildProperties(department, ldapGroups);

		return new User(username, username, null, firstName, lastName, UserState.REMOTE, groupsAndRoles.groups(),
				groupsAndRoles.roles(), locale, properties, false, UserHistory.EMPTY);
	}

	protected String validateLdapUsername(String username, Attributes attrs) throws NamingException {
		Attribute sAMAccountName = attrs.get(LDAP_SAM_ACCOUNT_NAME);
		if (sAMAccountName == null || !username.equalsIgnoreCase(sAMAccountName.get().toString()))
			throw new AccessDeniedException(
					"Could not login with user: " + username + this.domain + " on Ldap: Wrong LDAP Data");

		return sAMAccountName.get().toString();
	}

	protected String getLdapString(Attributes attrs, String key) throws NamingException {
		Attribute sn = attrs.get(key);
		return sn == null ? null : sn.get().toString();
	}

	protected String getFirstName(String username, Attributes attrs) throws NamingException {
		String value = getLdapString(attrs, LDAP_GIVEN_NAME);
		return isEmpty(value) ? username : value;
	}

	protected String getLastName(String username, Attributes attrs) throws NamingException {
		String value = getLdapString(attrs, LDAP_SN);
		return isEmpty(value) ? username : value;
	}

	protected Locale getLocale(Attributes attrs) {
		return this.defaultLocale;
	}

	protected Set<String> getLdapGroups(Attributes attrs) throws NamingException {
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
}
