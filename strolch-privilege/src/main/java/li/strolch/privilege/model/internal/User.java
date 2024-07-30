/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package li.strolch.privilege.model.internal;

import li.strolch.privilege.base.PrivilegeConstants;
import li.strolch.privilege.model.Group;
import li.strolch.privilege.model.UserRep;
import li.strolch.privilege.model.UserState;
import li.strolch.utils.dbc.DBC;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import static li.strolch.privilege.base.PrivilegeConstants.*;

/**
 * This class defines the actual login information for a given user which can be granted privileges. Every user is
 * granted a set of {@link Role}s and has a {@link UserState} including detail information like first name and lastname
 *
 * <p>
 * Note: This is an internal object which is not to be serialized or passed to clients, {@link UserRep}s are used for
 * that
 * </p>
 *
 * @param userId        the user's id
 * @param username      the user's login name
 * @param passwordCrypt the {@link PasswordCrypt} containing user's password information
 * @param firstname     the user's first name
 * @param lastname      the user's lastname
 * @param userState     the user's {@link UserState}
 * @param groups        the set of {@link Group}s assigned to this user
 * @param roles         the set of {@link Role}s assigned to this user
 * @param locale        the user's {@link Locale}
 * @param propertyMap   a {@link Map} containing string value pairs of properties for this user
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public record User(String userId, String username, PasswordCrypt passwordCrypt, String firstname, String lastname,
				   UserState userState, Set<String> groups, Set<String> roles, Locale locale,
				   Map<String, String> propertyMap, boolean passwordChangeRequested, UserHistory history) {

	public User(String userId, String username, PasswordCrypt passwordCrypt, String firstname, String lastname,
			UserState userState, Set<String> groups, Set<String> roles, Locale locale, Map<String, String> propertyMap,
			boolean passwordChangeRequested, UserHistory history) {

		DBC.PRE.assertNotEmpty("userId must not be empty", userId);
		DBC.PRE.assertNotEmpty("username must not be empty", username);
		DBC.PRE.assertNotNull("userState must not be null", userState);
		DBC.PRE.assertNotNull("history must not be null", history);

		if (userState != UserState.SYSTEM) {
			DBC.PRE.assertNotEmpty("lastname must not be empty when not system user!", username);
			DBC.PRE.assertNotEmpty("firstname must not be empty when not system user!", firstname);
		}

		// passwordCrypt may be null, meaning not able to login
		// roles may be null, meaning not able to login and must be added later
		// locale may be null, meaning use system default
		// properties may be null, meaning no properties

		this.userId = userId;

		this.username = username;
		this.passwordCrypt = passwordCrypt;

		this.userState = userState;

		this.firstname = firstname;
		this.lastname = lastname;

		this.groups = groups == null ? Set.of() : Set.copyOf(groups);
		this.roles = roles == null ? Set.of() : Set.copyOf(roles);
		this.locale = locale == null ? Locale.getDefault() : locale;
		this.propertyMap = propertyMap == null ? Map.of() : Map.copyOf(propertyMap);

		this.passwordChangeRequested = passwordChangeRequested;
		this.history = history;
	}

	public String getUserId() {
		return this.userId;
	}

	public String getUsername() {
		return this.username;
	}

	/**
	 * Returns the {@link PasswordCrypt} for this user, null if not password set
	 *
	 * @return the {@link PasswordCrypt} for this user, null if not password set
	 */
	public PasswordCrypt getPasswordCrypt() {
		return this.passwordCrypt;
	}

	public String getFirstname() {
		return this.firstname;
	}

	public String getLastname() {
		return this.lastname;
	}

	public UserState getUserState() {
		return this.userState;
	}

	public Set<String> getGroups() {
		return this.groups;
	}

	public Set<String> getRoles() {
		return this.roles;
	}

	public boolean hasGroup(String group) {
		return this.groups.contains(group);
	}

	public boolean hasRole(String role) {
		return this.roles.contains(role);
	}

	public Locale getLocale() {
		return this.locale;
	}

	public boolean isPasswordChangeRequested() {
		return this.passwordChangeRequested;
	}

	public UserHistory getHistory() {
		return this.history;
	}

	/**
	 * Returns true if the history for this user is empty
	 *
	 * @return true if the history for this user is empty
	 */
	public boolean isHistoryEmpty() {
		return this.history.isEmpty();
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

	/**
	 * Returns the value of the property {@link PrivilegeConstants#EMAIL}
	 *
	 * @return the value of the property {@link PrivilegeConstants#EMAIL}
	 */
	public String getEmail() {
		return getProperty(EMAIL);
	}

	/**
	 * @return a {@link UserRep} which is a representation of this object used to serialize and view on clients
	 */
	public UserRep asUserRep() {
		return new UserRep(this.userId, this.username, this.firstname, this.lastname, this.userState, this.groups,
				this.roles, this.locale, new HashMap<>(this.propertyMap), this.history);
	}

	/**
	 * Returns a string representation of this object displaying its concrete type and its values
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "User [userId=" + this.userId + ", username=" + this.username + ", firstname=" + this.firstname +
				", lastname=" + this.lastname + ", locale=" + this.locale + ", userState=" + this.userState +
				", roles=" + this.roles + ", groups=" + this.groups + "]";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.userId == null) ? 0 : this.userId.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		User other = (User) obj;
		if (this.userId == null)
			return other.userId == null;
		return this.userId.equals(other.userId);
	}

	public User withHistory(UserHistory history) {
		return new User(this.userId, this.username, this.passwordCrypt, this.firstname, this.lastname, this.userState,
				this.groups, this.roles, this.locale, this.propertyMap, this.passwordChangeRequested, history);
	}
}
