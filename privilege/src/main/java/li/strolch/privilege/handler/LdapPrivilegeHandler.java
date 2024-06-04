package li.strolch.privilege.handler;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.InvalidCredentialsException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.helper.LdapHelper;
import li.strolch.privilege.model.Group;
import li.strolch.privilege.model.UserState;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserHistory;
import li.strolch.privilege.policy.PrivilegePolicy;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.ExceptionHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.naming.*;
import javax.naming.directory.*;
import java.io.File;
import java.io.FileReader;
import java.util.*;
import java.util.concurrent.ScheduledExecutorService;

import static java.text.MessageFormat.format;
import static java.util.stream.Collectors.toSet;
import static li.strolch.privilege.base.PrivilegeConstants.*;
import static li.strolch.privilege.helper.LdapHelper.LDAP_DEPARTMENT;
import static li.strolch.privilege.helper.LdapHelper.LDAP_SN;
import static li.strolch.privilege.helper.ModelHelper.buildLocationProperties;
import static li.strolch.privilege.helper.XmlConstants.PARAM_BASE_PATH;
import static li.strolch.privilege.helper.XmlConstants.PARAM_CONFIG_FILE;
import static li.strolch.utils.LdapHelper.encodeForLDAP;
import static li.strolch.utils.helper.StringHelper.*;

public class LdapPrivilegeHandler extends DefaultPrivilegeHandler {

	protected static final Logger logger = LoggerFactory.getLogger(LdapPrivilegeHandler.class);
	public static final String LDAP_FILTER_TEMPLATE = "(&(objectCategory=person)(objectClass=user)(%s=%s)%s)";
	public static final String SAM_ACCOUNT_NAME = "sAMAccountName";
	public static final String USER_PRINCIPAL_NAME = "userPrincipalName";

	private String providerUrl;
	private String searchBase;
	private String additionalFilter;
	private String domain;
	private String domainPrefix;

	public static final String USER_GROUP_OVERRIDES = "userGroupOverrides";
	public static final String GROUP_MAPPINGS = "groupMappings";
	public static final String LOCATION_MAPPINGS = "locationMappings";
	public static final String GROUP_CONFIGS = "ldapGroupConfigs";
	public static final String LDAP_GIVEN_NAME = "givenName";

	private Locale defaultLocale;
	private Map<String, String> ldapGroupToLocalGroupMap;
	private Map<String, String> ldapLocationToLocalLocationMap;
	private JsonObject groupConfigs;
	private String realm;
	private HashMap<String, String> userGroupOverrides;

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

		this.realm = parameterMap.get(REALM);
		DBC.PRE.assertNotEmpty("realm must be set!", realm);

		this.defaultLocale = parameterMap.containsKey("defaultLocale") ?
				Locale.forLanguageTag(parameterMap.get("defaultLocale")) : Locale.getDefault();

		String configFileS = parameterMap.get(PARAM_CONFIG_FILE);
		DBC.PRE.assertNotEmpty("configFile param must be set!", configFileS);

		// see if we need a base path for our configuration file
		File configFile = new File(configFileS);
		if (!configFile.isAbsolute()) {
			String basePath = parameterMap.get(PARAM_BASE_PATH);
			if (isEmpty(basePath)) {
				String msg = "[{0}] Config file parameter {1} is not absolute, and base bath {2} is not set!";
				msg = format(msg, LdapPrivilegeHandler.class.getName(), PARAM_CONFIG_FILE, basePath);
				throw new PrivilegeException(msg);
			}

			File basePathF = new File(basePath);
			if (!basePathF.exists() && !basePathF.isDirectory()) {
				String msg = "[{0}] Config file parameter {1} is not absolute, and base bath {2} is not a directory!";
				msg = format(msg, LdapPrivilegeHandler.class.getName(), PARAM_CONFIG_FILE, basePathF.getAbsolutePath());
				throw new PrivilegeException(msg);
			}

			configFile = new File(basePath, configFile.getName());
		}
		if (!configFile.exists() || !configFile.isFile() || !configFile.canRead())
			throw new IllegalStateException("configFile does not exist, is not a file, or can not be read at path "
					+ configFile.getAbsolutePath());

		// parse the configuration file
		JsonObject configJ;
		try (FileReader reader = new FileReader(configFile)) {
			configJ = JsonParser.parseReader(reader).getAsJsonObject();
		} catch (Exception e) {
			throw new IllegalStateException("Failed to read config file " + configFile.getAbsolutePath(), e);
		}

		// validate the configuration
		if (!configJ.has(GROUP_CONFIGS) || !configJ.get(GROUP_CONFIGS).isJsonObject())
			throw new IllegalStateException("JSON config is missing ldapGroupConfigs element!");

		this.ldapLocationToLocalLocationMap = new HashMap<>();
		if (configJ.has(LOCATION_MAPPINGS)) {
			JsonObject locationMappingsJ = configJ.get(LOCATION_MAPPINGS).getAsJsonObject();
			for (String ldapLocation : locationMappingsJ.keySet()) {
				String localLocation = locationMappingsJ.get(ldapLocation).getAsString();
				this.ldapLocationToLocalLocationMap.put(ldapLocation, localLocation);
			}
		}
		this.ldapGroupToLocalGroupMap = new HashMap<>();
		if (configJ.has(GROUP_MAPPINGS)) {
			JsonObject groupMappingsJ = configJ.get(GROUP_MAPPINGS).getAsJsonObject();
			for (String ldapGroup : groupMappingsJ.keySet()) {
				String localGroup = groupMappingsJ.get(ldapGroup).getAsString();
				this.ldapGroupToLocalGroupMap.put(ldapGroup, localGroup);
			}
		}

		this.groupConfigs = configJ.get(GROUP_CONFIGS).getAsJsonObject();

		// validate the configuration
		for (String name : this.groupConfigs.keySet()) {
			JsonObject config = this.groupConfigs.get(name).getAsJsonObject();
			JsonElement locationsJ = config.get(LOCATIONS);
			if (locationsJ == null || !locationsJ.isJsonArray() || locationsJ.getAsJsonArray().isEmpty())
				throw new IllegalStateException(
						format("LDAP Group {0} is missing attribute {1}, or it is not an array or the array is empty",
								name, LOCATIONS));
			JsonElement rolesJ = config.get(ROLES);
			if (rolesJ == null || !rolesJ.isJsonArray() || rolesJ.getAsJsonArray().isEmpty())
				throw new IllegalStateException(
						format("LDAP Group {0} is missing attribute {1}, or it is not an array or the array is empty",
								name, ROLES));
		}

		this.userGroupOverrides = new HashMap<>();
		if (configJ.has(USER_GROUP_OVERRIDES)) {
			JsonObject userGroupOverrides = configJ.get(USER_GROUP_OVERRIDES).getAsJsonObject();
			for (String username : userGroupOverrides.keySet()) {
				String group = userGroupOverrides.get(username).getAsString();
				logger.info("Registered user group override for user {} to group {}", username, group);
				this.userGroupOverrides.put(username, group);
			}
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
			logger.error("Could not login with user: {} on Ldap", safeUsername, e);
			throw new InvalidCredentialsException("Could not login with user: " + safeUsername + " on Ldap", e);
		} catch (Exception e) {
			logger.error("Could not login with user: {} on Ldap", safeUsername, e);
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

					logger.warn("No LDAP data retrieved using {}, trying with {}...", SAM_ACCOUNT_NAME,
							USER_PRINCIPAL_NAME);
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
		logger.info("User {} is member of the following LDAP groups: ", username);
		ldapGroups.forEach(s -> logger.info("- {}", s));
		GroupsAndRoles groupsAndRoles = mapToStrolchGroupsAndRoles(ldapGroups);

		Map<String, String> properties = buildProperties(attrs, ldapGroups);

		return new User(username, username, null, firstName, lastName, UserState.REMOTE, groupsAndRoles.groups,
				groupsAndRoles.roles, locale, properties, false, UserHistory.EMPTY);
	}

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

	protected Set<String> getLdapGroups(String username, Attributes attrs) throws NamingException {
		Set<String> ldapGroups = LdapHelper.getLdapGroups(attrs);
		logger.info("User {} has LDAP Groups: ", username);
		ldapGroups.forEach(s -> logger.info("- {}", s));

		if (this.userGroupOverrides.containsKey(username)) {
			String overrideGroup = this.userGroupOverrides.get(username);
			ldapGroups.clear();
			ldapGroups.add(overrideGroup);
			logger.info("Overriding LDAP group for user {} to {}", username, overrideGroup);
		}

		return ldapGroups;
	}

	protected GroupsAndRoles mapToStrolchGroupsAndRoles(Set<String> ldapGroups) {

		// first see if we have mappings for LDAP groups to local groups
		Set<String> mappedGroupNames = ldapGroups
				.stream()
				.map(lg -> this.ldapGroupToLocalGroupMap.getOrDefault(lg, lg))
				.collect(toSet());

		// now see if we have any groups with these names
		Set<String> groups = getPersistenceHandler()
				.getAllGroups()
				.stream()
				.map(Group::name)
				.filter(mappedGroupNames::contains)
				.collect(toSet());

		// now map any LDAP groups to roles
		Set<String> roles = new HashSet<>();
		for (String relevantLdapGroup : ldapGroups) {
			JsonObject mappingJ = this.groupConfigs.get(relevantLdapGroup).getAsJsonObject();
			if (mappingJ.has(ROLES))
				mappingJ.get(ROLES).getAsJsonArray().forEach(e -> roles.add(e.getAsString()));
		}

		return new GroupsAndRoles(groups, roles);
	}

	protected Map<String, String> buildProperties(Attributes attrs, Set<String> ldapGroups) throws NamingException {

		String primaryLocation = "";
		Set<String> secondaryLocations = new HashSet<>();
		Set<String> organisations = new HashSet<>();
		Set<String> locations = new HashSet<>();

		// first see if we can find the primaryLocation from the department attribute:
		String department = getLdapString(attrs, LDAP_DEPARTMENT);
		if (isNotEmpty(department)) {
			if (this.ldapLocationToLocalLocationMap.containsKey(department)) {
				String localL = this.ldapLocationToLocalLocationMap.get(department);
				logger.info("Using primary location {} for LDAP department {}", localL, department);
				primaryLocation = localL;
			}
		}

		for (String ldapGroup : ldapGroups) {
			JsonObject mappingJ = this.groupConfigs.get(ldapGroup).getAsJsonObject();
			if (mappingJ.has(ORGANISATION))
				mappingJ.get(ORGANISATION).getAsJsonArray().forEach(e -> organisations.add(e.getAsString()));
			mappingJ.get(LOCATION).getAsJsonArray().forEach(e -> locations.add(e.getAsString()));

			JsonElement primaryLocationJ = mappingJ.get(PRIMARY_LOCATION);
			if (primaryLocationJ != null && !primaryLocationJ.isJsonNull()) {
				if (primaryLocation.isEmpty()) {
					primaryLocation = primaryLocationJ.getAsString();
				} else {
					String location = primaryLocationJ.getAsString();
					if (!secondaryLocations.contains(location)) {
						logger.warn(
								"Primary location already set by previous LDAP Group config for LDAP Group {}, adding to secondary locations.",
								ldapGroup);
						secondaryLocations.add(location);
					}
				}
			}

			JsonElement secondaryLocationsJ = mappingJ.get(SECONDARY_LOCATIONS);
			if (secondaryLocationsJ != null && !secondaryLocationsJ.isJsonNull()) {
				if (secondaryLocations.isEmpty()) {
					if (secondaryLocationsJ.isJsonPrimitive())
						secondaryLocations.add(secondaryLocationsJ.getAsString());
					else
						secondaryLocationsJ.getAsJsonArray().forEach(s -> secondaryLocations.add(s.getAsString()));
				} else {
					logger.warn(
							"Secondary locations already set by previous LDAP Group config for LDAP Group {}, adding additional",
							ldapGroup);
					if (secondaryLocationsJ.isJsonPrimitive())
						secondaryLocations.add(secondaryLocationsJ.getAsString());
					else
						secondaryLocationsJ.getAsJsonArray().forEach(s -> secondaryLocations.add(s.getAsString()));
				}
			}
		}

		return buildLocationProperties(this.realm, organisations, locations, primaryLocation, secondaryLocations);
	}

	public record GroupsAndRoles(Set<String> groups, Set<String> roles) {
	}
}
