package li.strolch.privilege.handler;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import li.strolch.privilege.helper.LdapHelper;
import li.strolch.privilege.policy.PrivilegePolicy;
import li.strolch.utils.dbc.DBC;

import javax.naming.NamingException;
import javax.naming.directory.Attributes;
import java.io.File;
import java.io.FileReader;
import java.util.*;
import java.util.concurrent.ScheduledExecutorService;

import static java.lang.String.join;
import static java.util.stream.Collectors.toSet;
import static li.strolch.privilege.base.PrivilegeConstants.*;
import static li.strolch.utils.helper.StringHelper.isEmpty;
import static li.strolch.utils.helper.StringHelper.isNotEmpty;

public class JsonConfigLdapPrivilegeHandler extends BaseLdapPrivilegeHandler {

	private Locale defaultLocale;
	private Map<String, String> ldapToLocalLocationMap;
	private JsonObject ldapGroupConfigs;
	private Set<String> ldapGroupNames;
	private String realm;
	private HashMap<String, String> userLdapGroupOverrides;

	@Override
	public void initialize(ScheduledExecutorService executorService, Map<String, String> parameterMap,
			EncryptionHandler encryptionHandler, PasswordStrengthHandler passwordStrengthHandler,
			PersistenceHandler persistenceHandler, UserChallengeHandler userChallengeHandler,
			SingleSignOnHandler ssoHandler, Map<String, Class<PrivilegePolicy>> policyMap) {

		super.initialize(executorService, parameterMap, encryptionHandler, passwordStrengthHandler, persistenceHandler,
				userChallengeHandler, ssoHandler, policyMap);

		this.realm = parameterMap.get(REALM);
		DBC.PRE.assertNotEmpty("realm must be set!", realm);

		this.defaultLocale = parameterMap.containsKey("defaultLocale") ?
				Locale.forLanguageTag(parameterMap.get("defaultLocale")) : Locale.getDefault();

		String configFileS = parameterMap.get("configFile");
		DBC.PRE.assertNotEmpty("configFile param must be set!", configFileS);
		File configFile = new File(configFileS);
		if (!configFile.exists() || !configFile.isFile() || !configFile.canRead())
			throw new IllegalStateException("configFile does not exist, is not a file, or can not be read at path " +
					configFile.getAbsolutePath());

		// parse the configuration file
		JsonObject configJ;
		try (FileReader reader = new FileReader(configFile)) {
			configJ = JsonParser.parseReader(reader).getAsJsonObject();
		} catch (Exception e) {
			throw new IllegalStateException("Failed to read config file " + configFile.getAbsolutePath(), e);
		}

		// validate the configuration
		if (!configJ.has("ldapGroupConfigs") || !configJ.get("ldapGroupConfigs").isJsonObject())
			throw new IllegalStateException("JSON config is missing ldapGroupConfigs element!");

		this.ldapToLocalLocationMap = new HashMap<>();
		if (configJ.has("locationMappings")) {
			JsonObject locationMappingsJ = configJ.get("locationMappings").getAsJsonObject();
			for (String ldapL : locationMappingsJ.keySet()) {
				String localL = locationMappingsJ.get(ldapL).getAsString();
				this.ldapToLocalLocationMap.put(ldapL, localL);
			}
		}

		this.ldapGroupConfigs = configJ.get("ldapGroupConfigs").getAsJsonObject();
		this.ldapGroupNames = ldapGroupConfigs.keySet();
		if (this.ldapGroupNames.isEmpty())
			throw new IllegalStateException(
					"No LDAP group names are defined in config file " + configFile.getAbsolutePath());

		// validate the configuration
		for (String name : this.ldapGroupNames) {
			JsonObject config = ldapGroupConfigs.get(name).getAsJsonObject();
			if (!config.has(LOCATION) || !config.get(LOCATION).isJsonArray() ||
					config.get(LOCATION).getAsJsonArray().isEmpty())
				throw new IllegalStateException("LDAP Group " + name +
						" is missing a location attribute, or it is not an array or the array is empty");
			if (!config.has(LOCATION) || !config.get(LOCATION).isJsonArray() ||
					config.get(LOCATION).getAsJsonArray().isEmpty())
				throw new IllegalStateException("LDAP Group " + name +
						" is missing a roles attribute, or it is not an array or the array is empty");
		}

		this.userLdapGroupOverrides = new HashMap<>();
		if (configJ.has("userLdapGroupOverrides")) {
			JsonObject userLdapGroupOverrides = configJ.get("userLdapGroupOverrides").getAsJsonObject();
			for (String username : userLdapGroupOverrides.keySet()) {
				String group = userLdapGroupOverrides.get(username).getAsString();
				logger.info("Registered LDAP group override for user " + username + " to group " + group);
				this.userLdapGroupOverrides.put(username, group);
			}
		}
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
	protected Locale getLocale(Attributes attrs) {
		return this.defaultLocale;
	}

	@Override
	protected Set<String> getLdapGroups(String username, Attributes attrs) throws NamingException {
		Set<String> ldapGroups = LdapHelper.getLdapGroups(attrs);
		logger.info("User " + username + " has LDAP Groups: ");
		ldapGroups.forEach(s -> logger.info("- " + s));

		if (this.userLdapGroupOverrides.containsKey(username)) {
			String overrideGroup = this.userLdapGroupOverrides.get(username);
			ldapGroups.clear();
			ldapGroups.add(overrideGroup);
			logger.info("Overriding LDAP group for user " + username + " to " + overrideGroup);
		}

		Set<String> relevantLdapGroups = ldapGroups.stream().filter(s -> this.ldapGroupNames.contains(s))
				.collect(toSet());
		if (relevantLdapGroups.isEmpty())
			throw new IllegalStateException("User " + username +
					" can not login, as none of their LDAP Groups have mappings to Strolch Roles!");

		if (relevantLdapGroups.size() > 1) {
			logger.warn(
					"User " + username + " has multiple relevant LDAP Groups which will lead to undefined behaviour: " +
							join(",", relevantLdapGroups));
		}

		return relevantLdapGroups;
	}

	@Override
	protected Set<String> mapToStrolchGroups(String username, Set<String> ldapGroups) {
		return mapLdapGroupToStrolch(ldapGroups, GROUPS);
	}

	@Override
	protected Set<String> mapToStrolchRoles(String username, Set<String> ldapGroups) {
		return mapLdapGroupToStrolch(ldapGroups, ROLES);
	}

	private Set<String> mapLdapGroupToStrolch(Set<String> ldapGroups, String type) {
		Set<String> mappedValues = new HashSet<>();
		for (String relevantLdapGroup : ldapGroups) {
			JsonObject mappingJ = this.ldapGroupConfigs.get(relevantLdapGroup).getAsJsonObject();
			if (mappingJ.has(type))
				mappingJ.get(type).getAsJsonArray().forEach(e -> mappedValues.add(e.getAsString()));
		}

		return mappedValues;
	}

	@Override
	protected Map<String, String> buildProperties(String username, Attributes attrs, Set<String> ldapGroups,
			Set<String> strolchRoles) throws NamingException {

		String primaryLocation = "";
		Set<String> secondaryLocations = new HashSet<>();
		Set<String> organisations = new HashSet<>();
		Set<String> locations = new HashSet<>();

		// first see if we can find the primaryLocation from the department attribute:
		String department = getLdapString(attrs, "department");
		if (isNotEmpty(department)) {
			if (this.ldapToLocalLocationMap.containsKey(department)) {
				String localL = this.ldapToLocalLocationMap.get(department);
				logger.info("Using primary location " + localL + " for LDAP department " + department);
				primaryLocation = localL;
			}
		}

		for (String ldapGroup : ldapGroups) {
			JsonObject mappingJ = this.ldapGroupConfigs.get(ldapGroup).getAsJsonObject();
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
						logger.warn("Primary location already set by previous LDAP Group config for LDAP Group " +
								ldapGroup + ", adding to secondary locations.");
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
					logger.warn("Secondary locations already set by previous LDAP Group config for LDAP Group " +
							ldapGroup + ", adding additional");
					if (secondaryLocationsJ.isJsonPrimitive())
						secondaryLocations.add(secondaryLocationsJ.getAsString());
					else
						secondaryLocationsJ.getAsJsonArray().forEach(s -> secondaryLocations.add(s.getAsString()));
				}
			}
		}

		Map<String, String> properties = new HashMap<>();
		properties.put(REALM, this.realm);
		if (!organisations.isEmpty())
			properties.put(ORGANISATION, join(",", organisations));
		properties.put(LOCATION, join(",", locations));
		properties.put(PRIMARY_LOCATION, primaryLocation);
		properties.put(SECONDARY_LOCATIONS, join(",", secondaryLocations));
		return properties;
	}
}
