package li.strolch.privilege.helper;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.handler.LdapPrivilegeHandler;
import li.strolch.privilege.handler.PersistenceHandler;
import li.strolch.privilege.model.Group;
import li.strolch.utils.dbc.DBC;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static java.text.MessageFormat.format;
import static java.util.stream.Collectors.toSet;
import static li.strolch.privilege.base.PrivilegeConstants.*;
import static li.strolch.privilege.helper.ModelHelper.buildLocationProperties;
import static li.strolch.privilege.helper.XmlConstants.PARAM_CONFIG_FILE;
import static li.strolch.utils.helper.StringHelper.isEmpty;
import static li.strolch.utils.helper.StringHelper.isNotEmpty;

public class RemoteGroupMappingModel {

	private static final Logger logger = LoggerFactory.getLogger(RemoteGroupMappingModel.class);

	public static final String USER_GROUP_OVERRIDES = "userGroupOverrides";
	public static final String GROUP_MAPPINGS = "groupMappings";
	public static final String LOCATION_MAPPINGS = "locationMappings";
	public static final String GROUP_CONFIGS = "groupConfigs";

	private final PersistenceHandler persistenceHandler;

	private String realm;
	private Map<String, String> remoteGroupToLocalGroupMap;
	private Map<String, String> remoteLocationToLocalLocationMap;
	private JsonObject groupConfigs;
	private HashMap<String, String> userGroupOverrides;

	public RemoteGroupMappingModel(PersistenceHandler persistenceHandler) {
		this.persistenceHandler = persistenceHandler;
	}

	public void loadJsonConfig(String realm, String basePath, String configFileS) {
		DBC.PRE.assertNotEmpty("realm param must be set!", realm);
		DBC.PRE.assertNotEmpty("configFile param must be set!", configFileS);
		this.realm = realm;

		// see if we need a base path for our configuration file
		File configFile = new File(configFileS);
		if (!configFile.isAbsolute()) {
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

		this.remoteLocationToLocalLocationMap = new HashMap<>();
		if (configJ.has(LOCATION_MAPPINGS)) {
			JsonObject locationMappingsJ = configJ.get(LOCATION_MAPPINGS).getAsJsonObject();
			for (String ldapLocation : locationMappingsJ.keySet()) {
				String localLocation = locationMappingsJ.get(ldapLocation).getAsString();
				this.remoteLocationToLocalLocationMap.put(ldapLocation, localLocation);
			}
		}
		this.remoteGroupToLocalGroupMap = new HashMap<>();
		if (configJ.has(GROUP_MAPPINGS)) {
			JsonObject groupMappingsJ = configJ.get(GROUP_MAPPINGS).getAsJsonObject();
			for (String ldapGroup : groupMappingsJ.keySet()) {
				String localGroup = groupMappingsJ.get(ldapGroup).getAsString();
				this.remoteGroupToLocalGroupMap.put(ldapGroup, localGroup);
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

	private Set<String> getGroupOverride(String username, Set<String> remoteGroups) {
		if (!this.userGroupOverrides.containsKey(username))
			return remoteGroups;

		String overrideGroup = this.userGroupOverrides.get(username);
		logger.info("Overriding remote group for user {} to {}", username, overrideGroup);
		return Set.of(overrideGroup);
	}

	public GroupsAndRoles mapRemoteGroupsToStrolch(String username, Set<String> remoteGroups) {
		if (this.userGroupOverrides.containsKey(username))
			remoteGroups = getGroupOverride(username, remoteGroups);

		// first see if we have mappings for LDAP groups to local groups
		Set<String> mappedGroupNames = remoteGroups
				.stream()
				.map(lg -> this.remoteGroupToLocalGroupMap.getOrDefault(lg, lg))
				.collect(toSet());

		// now see if we have any groups with these names
		Set<String> groups = this.persistenceHandler
				.getAllGroups()
				.stream()
				.map(Group::name)
				.filter(mappedGroupNames::contains)
				.collect(toSet());

		// now map any LDAP groups to roles
		Set<String> roles = new HashSet<>();
		for (String relevantLdapGroup : remoteGroups) {
			JsonObject mappingJ = this.groupConfigs.get(relevantLdapGroup).getAsJsonObject();
			if (mappingJ.has(ROLES))
				mappingJ.get(ROLES).getAsJsonArray().forEach(e -> roles.add(e.getAsString()));
		}

		return new GroupsAndRoles(groups, roles);
	}

	public Map<String, String> buildProperties(String remoteLocation, Set<String> remoteGroups) {
		Set<String> secondaryLocations = new HashSet<>();
		Set<String> organisations = new HashSet<>();
		Set<String> locations = new HashSet<>();

		String primaryLocation = "";
		if (isNotEmpty(remoteLocation)) {
			if (this.remoteLocationToLocalLocationMap.containsKey(remoteLocation)) {
				String localL = this.remoteLocationToLocalLocationMap.get(remoteLocation);
				logger.info("Using primary location {} for LDAP department {}", localL, remoteLocation);
				primaryLocation = localL;
			}
		}

		for (String ldapGroup : remoteGroups) {
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
}
