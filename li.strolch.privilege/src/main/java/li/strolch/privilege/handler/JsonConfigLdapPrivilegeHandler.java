package li.strolch.privilege.handler;

import static java.lang.String.join;
import static java.util.stream.Collectors.toSet;
import static li.strolch.privilege.base.PrivilegeConstants.*;
import static li.strolch.utils.helper.StringHelper.isEmpty;

import javax.naming.NamingException;
import javax.naming.directory.Attributes;
import java.io.File;
import java.io.FileReader;
import java.util.*;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import li.strolch.privilege.helper.LdapHelper;
import li.strolch.privilege.policy.PrivilegePolicy;
import li.strolch.utils.dbc.DBC;

public class JsonConfigLdapPrivilegeHandler extends BaseLdapPrivilegeHandler {

	private Locale defaultLocale;
	private JsonObject configJ;
	private Set<String> ldapGroupNames;
	private String realm;

	@Override
	public synchronized void initialize(Map<String, String> parameterMap, EncryptionHandler encryptionHandler,
			PersistenceHandler persistenceHandler, UserChallengeHandler userChallengeHandler,
			SingleSignOnHandler ssoHandler, Map<String, Class<PrivilegePolicy>> policyMap) {

		super.initialize(parameterMap, encryptionHandler, persistenceHandler, userChallengeHandler, ssoHandler,
				policyMap);

		this.realm = parameterMap.get(REALM);
		DBC.PRE.assertNotEmpty("realm must be set!", realm);

		this.defaultLocale = parameterMap.containsKey("defaultLocale") ?
				Locale.forLanguageTag(parameterMap.get("defaultLocale")) :
				Locale.getDefault();

		String configFileS = parameterMap.get("configFile");
		DBC.PRE.assertNotEmpty("configFile param must be set!", configFileS);
		File configFile = new File(configFileS);
		if (!configFile.exists() || !configFile.isFile() || !configFile.canRead())
			throw new IllegalStateException(
					"configFile does not exist, is not a file, or can not be read at path " + configFile
							.getAbsolutePath());

		// parse the configuration file
		try (FileReader reader = new FileReader(configFile)) {
			this.configJ = new JsonParser().parse(reader).getAsJsonObject();
		} catch (Exception e) {
			throw new IllegalStateException("Failed to read config file " + configFile.getAbsolutePath(), e);
		}

		// validate the configuration
		this.ldapGroupNames = this.configJ.keySet();
		if (this.ldapGroupNames.isEmpty())
			throw new IllegalStateException(
					"No LDAP group names are defined in config file " + configFile.getAbsolutePath());

		// validate the configuration
		for (String name : this.ldapGroupNames) {
			JsonObject config = this.configJ.get(name).getAsJsonObject();
			if (!config.has(LOCATION) || !config.get(LOCATION).isJsonArray()
					|| config.get(LOCATION).getAsJsonArray().size() == 0)
				throw new IllegalStateException("LDAP Group " + name
						+ " is missing a location attribute, or it is not an array or the array is empty");
			if (!config.has(LOCATION) || !config.get(LOCATION).isJsonArray()
					|| config.get(LOCATION).getAsJsonArray().size() == 0)
				throw new IllegalStateException("LDAP Group " + name
						+ " is missing a roles attribute, or it is not an array or the array is empty");
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
	protected Locale getLocale(Attributes attrs) throws NamingException {
		return this.defaultLocale;
	}

	@Override
	protected Set<String> getLdapGroups(String username, Attributes attrs) throws NamingException {
		Set<String> ldapGroups = LdapHelper.getLdapGroups(attrs);

		Set<String> relevantLdapGroups = ldapGroups.stream().filter(s -> this.ldapGroupNames.contains(s))
				.collect(toSet());
		if (relevantLdapGroups.isEmpty())
			throw new IllegalStateException("User " + username
					+ " can not login, as none of their LDAP Groups have mappings to Strolch Roles!");

		if (relevantLdapGroups.size() > 1) {
			logger.warn(
					"User " + username + " has multiple relevant LDAP Groups which will lead to undefined behaviour: "
							+ join(",", relevantLdapGroups));
		}

		return relevantLdapGroups;
	}

	@Override
	protected Set<String> mapToStrolchRoles(String username, Set<String> ldapGroups) {

		Set<String> strolchRoles = new HashSet<>();

		for (String relevantLdapGroup : ldapGroups) {
			JsonObject mappingJ = this.configJ.get(relevantLdapGroup).getAsJsonObject();
			mappingJ.get(ROLES).getAsJsonArray().forEach(e -> strolchRoles.add(e.getAsString()));
		}

		return strolchRoles;
	}

	@Override
	protected Map<String, String> buildProperties(String username, Attributes attrs, Set<String> ldapGroups,
			Set<String> strolchRoles) throws NamingException {

		String defaultLocation = "";
		Set<String> locations = new HashSet<>();

		for (String ldapGroup : ldapGroups) {
			JsonObject mappingJ = this.configJ.get(ldapGroup).getAsJsonObject();
			mappingJ.get(LOCATION).getAsJsonArray().forEach(e -> locations.add(e.getAsString()));
			JsonElement defaultLocationJ = mappingJ.get(DEFAULT_LOCATION);
			if (defaultLocationJ != null && !defaultLocationJ.isJsonNull()) {
				if (!defaultLocation.isEmpty())
					logger.warn("Default location already set by previous LDAP Group config, overriding for LDAP Group "
							+ ldapGroup);
				defaultLocation = defaultLocationJ.getAsString();
			}
		}

		Map<String, String> properties = new HashMap<>();
		properties.put(REALM, this.realm);
		properties.put(LOCATION, join(",", locations));
		properties.put(DEFAULT_LOCATION, defaultLocation);
		return properties;
	}
}
