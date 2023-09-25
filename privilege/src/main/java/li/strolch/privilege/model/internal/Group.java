package li.strolch.privilege.model.internal;

import li.strolch.privilege.base.PrivilegeConstants;
import li.strolch.utils.dbc.DBC;

import java.util.Map;
import java.util.Set;

import static li.strolch.privilege.base.PrivilegeConstants.*;

/**
 * This entity represents a group with which {@link User Users} can be associated. This allows to put roles and
 * properties which are always duplicated on users on to the group, and the User is then in the group, eliminating
 * duplication.
 */
public record Group(String name, Set<String> roles, Map<String, String> propertyMap) {
	public Group(String name, Set<String> roles, Map<String, String> propertyMap) {
		DBC.PRE.assertNotEmpty("name must not be empty", name);
		DBC.PRE.assertNotNull("roles must not be null", roles);
		DBC.PRE.assertNotNull("propertyMap must not be null", propertyMap);
		this.name = name;
		this.roles = Set.copyOf(roles);
		this.propertyMap = Map.copyOf(propertyMap);
	}

	/**
	 * Returns true if this group has the specified role
	 *
	 * @param role the name of the {@link Role} to check for
	 *
	 * @return true if this group has the specified role
	 */
	public boolean hasRole(String role) {
		return this.roles.contains(role);
	}

	/**
	 * Returns the property with the given key
	 *
	 * @param key the key for which the property is to be returned
	 *
	 * @return the property with the given key, or null if the property is not defined
	 */
	public String getProperty(String key) {
		return this.propertyMap.get(key);
	}

	/**
	 * Returns the {@link Set} of keys of all properties
	 *
	 * @return the {@link Set} of keys of all properties
	 */
	public Set<String> getPropertyKeySet() {
		return this.propertyMap.keySet();
	}

	/**
	 * Returns the map of properties
	 *
	 * @return the map of properties
	 */
	public Map<String, String> getProperties() {
		return this.propertyMap;
	}

	/**
	 * Returns the value of the property {@link PrivilegeConstants#REALM}
	 *
	 * @return the value of the property {@link PrivilegeConstants#REALM}
	 */
	public String getRealm() {
		return getProperty(REALM);
	}

	/**
	 * Returns the value of the property {@link PrivilegeConstants#ORGANISATION}
	 *
	 * @return the value of the property {@link PrivilegeConstants#ORGANISATION}
	 */
	public String getOrganisation() {
		return getProperty(ORGANISATION);
	}

	/**
	 * Returns the value of the property {@link PrivilegeConstants#LOCATION}
	 *
	 * @return the value of the property {@link PrivilegeConstants#LOCATION}
	 */
	public String getLocation() {
		return getProperty(LOCATION);
	}
}
