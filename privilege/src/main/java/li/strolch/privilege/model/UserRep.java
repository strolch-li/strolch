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
package li.strolch.privilege.model;

import li.strolch.privilege.base.PrivilegeConstants;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserHistory;
import li.strolch.utils.dbc.DBC;

import java.text.MessageFormat;
import java.util.*;

import static li.strolch.privilege.base.PrivilegeConstants.*;
import static li.strolch.utils.helper.StringHelper.isEmpty;
import static li.strolch.utils.helper.StringHelper.trimOrEmpty;

/**
 * To keep certain details of the {@link User} itself hidden from remote clients and make sure instances are only edited
 * by users with the correct privilege, this representational version is allowed to be viewed by remote clients and
 * simply wraps all public data from the {@link User}
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class UserRep {

	private String userId;
	private String username;
	private String firstname;
	private String lastname;
	private UserState userState;
	private Locale locale;
	private Set<String> groups;
	private Set<String> roles;
	private Map<String, String> properties;

	private UserHistory history;

	private boolean readOnly;

	/**
	 * Default constructor
	 *
	 * @param userId      the user's id
	 * @param username    the user's login name
	 * @param firstname   the user's first name
	 * @param lastname    the user's last name
	 * @param userState   the user's {@link UserState}
	 * @param groups      the set of {@link li.strolch.privilege.model.internal.Group}s assigned to this user
	 * @param roles       the set of {@link Role}s assigned to this user
	 * @param locale      the user's {@link Locale}
	 * @param propertyMap a {@link Map} containing string value pairs of properties for this user
	 */
	public UserRep(String userId, String username, String firstname, String lastname, UserState userState,
			Set<String> groups, Set<String> roles, Locale locale, Map<String, String> propertyMap,
			UserHistory history) {
		this.userId = trimOrEmpty(userId);
		this.username = trimOrEmpty(username);
		this.firstname = trimOrEmpty(firstname);
		this.lastname = trimOrEmpty(lastname);
		this.userState = userState;
		this.locale = locale;
		setGroups(groups == null ? Set.of() : groups);
		setRoles(roles == null ? Set.of() : roles);
		setProperties(properties == null ? Map.of() : properties);
		this.history = history == null ? UserHistory.EMPTY : history;
	}

	@SuppressWarnings("unused")
	private UserRep() {
		// No arg constructor for JAXB
	}

	/**
	 * Validates that all required fields are set
	 */
	public void validate() {
		if (isEmpty(this.userId))
			throw new PrivilegeException("userId must not be empty");
		if (isEmpty(this.username))
			throw new PrivilegeException("username must not be empty");

		// username must be at least 3 characters in length
		if (this.username.length() < 3) {
			String msg = MessageFormat.format("The given username ''{0}'' is shorter than 3 characters", this.username);
			throw new PrivilegeException(msg);
		}

		if (this.userState == null)
			throw new PrivilegeException("userState may not be null");

		if (this.userState != UserState.SYSTEM) {
			if (isEmpty(this.firstname))
				throw new PrivilegeException("firstname may not be empty for non-system users");
			if (isEmpty(this.lastname))
				throw new PrivilegeException("lastname may not be empty for non-system users");
		}

		if (this.groups == null)
			throw new PrivilegeException("groups may not be null");
		if (this.roles == null)
			throw new PrivilegeException("roles may not be null");

		if (this.groups.isEmpty() && this.roles.isEmpty())
			throw new PrivilegeException("User must have at least one group or role assigned!");
	}

	public boolean isReadOnly() {
		return readOnly;
	}

	public UserRep readOnly() {
		if (this.readOnly)
			return this;
		this.readOnly = true;
		this.groups = Set.copyOf(this.groups);
		this.roles = Set.copyOf(this.roles);
		this.properties = Map.copyOf(this.properties);
		return this;
	}

	protected void assertNotReadonly() {
		if (this.readOnly)
			throw new IllegalStateException("User is currently readOnly, to modify get a copy!");
	}

	public boolean isSystemUser() {
		return this.userState.isSystem();
	}

	public boolean isRemoteUser() {
		return this.userState.isRemote();
	}

	public boolean isNormalEnabledUser() {
		return this.userState.isNormalEnabledUser();
	}

	/**
	 * @return the userId
	 */
	public String getUserId() {
		return this.userId;
	}

	/**
	 * Set the userId
	 *
	 * @param userId to set
	 */
	public void setUserId(String userId) {
		assertNotReadonly();
		this.userId = trimOrEmpty(userId);
	}

	/**
	 * @return the username
	 */
	public String getUsername() {
		return this.username;
	}

	/**
	 * @param username the username to set
	 */
	public void setUsername(String username) {
		assertNotReadonly();
		this.username = trimOrEmpty(username);
	}

	/**
	 * @return the firstname
	 */
	public String getFirstname() {
		return this.firstname;
	}

	/**
	 * @param firstname the firstname to set
	 */
	public void setFirstname(String firstname) {
		assertNotReadonly();
		this.firstname = trimOrEmpty(firstname);
	}

	/**
	 * @return the lastname
	 */
	public String getLastname() {
		return this.lastname;
	}

	/**
	 * @param lastname the lastname to set
	 */
	public void setLastname(String lastname) {
		assertNotReadonly();
		this.lastname = trimOrEmpty(lastname);
	}

	/**
	 * @return the userState
	 */
	public UserState getUserState() {
		return this.userState;
	}

	/**
	 * @param userState the userState to set
	 */
	public void setUserState(UserState userState) {
		assertNotReadonly();
		this.userState = userState;
	}

	public Set<String> getGroups() {
		return this.groups;
	}

	public void setGroups(Set<String> groups) {
		DBC.PRE.assertNotNull("groups must not be null!", groups);
		assertNotReadonly();
		this.groups = groups.stream().map(String::trim).collect(HashSet::new, HashSet::add, HashSet::addAll);
	}

	public boolean hasGroup(String group) {
		return this.groups.contains(group);
	}

	/**
	 * @return the roles
	 */
	public Set<String> getRoles() {
		return this.roles;
	}

	/**
	 * @param roles the roles to set
	 */
	public void setRoles(Set<String> roles) {
		DBC.PRE.assertNotNull("roles must not be null!", roles);
		assertNotReadonly();
		this.roles = roles.stream().map(String::trim).collect(HashSet::new, HashSet::add, HashSet::addAll);
	}

	public void addRole(String role) {
		assertNotReadonly();
		this.roles.add(role);
	}

	public boolean hasRole(String role) {
		return this.roles.contains(role);
	}

	/**
	 * @return the locale
	 */
	public Locale getLocale() {
		return this.locale;
	}

	/**
	 * @param locale the locale to set
	 */
	public void setLocale(Locale locale) {
		assertNotReadonly();
		this.locale = locale;
	}

	/**
	 * Returns the {@link UserHistory}
	 *
	 * @return the user history
	 */
	public UserHistory getHistory() {
		return this.history;
	}

	/**
	 * Returns true if the given property exists
	 *
	 * @param key the property key to check
	 *
	 * @return true if the given property exists
	 */
	public boolean hasProperty(String key) {
		return this.properties.containsKey(key);
	}

	/**
	 * Returns the property with the given key
	 *
	 * @param key the key for which the property is to be returned
	 *
	 * @return the property with the given key, or null if the property is not defined
	 */
	public String getProperty(String key) {
		return this.properties.get(key);
	}

	/**
	 * Set the property with the key to the value
	 *
	 * @param key   the key of the property to set
	 * @param value the value of the property to set
	 */
	public void setProperty(String key, String value) {
		DBC.PRE.assertNotEmpty("key must not be empty!", key);
		DBC.PRE.assertNotEmpty("value must not be empty!", value);
		assertNotReadonly();
		this.properties.put(key.trim(), value.trim());
	}

	public void setProperties(Map<String, String> properties) {
		DBC.PRE.assertNotNull("properties must not be null!", properties);
		assertNotReadonly();
		this.properties = new HashMap<>();
		properties.forEach((key, value) -> this.properties.put(key.trim(), value.trim()));
	}

	/**
	 * Returns the {@link Set} of keys of all properties
	 *
	 * @return the {@link Set} of keys of all properties
	 */
	public Set<String> getPropertyKeySet() {
		return this.properties.keySet();
	}

	/**
	 * Returns the map of properties
	 *
	 * @return the map of properties
	 */
	public Map<String, String> getProperties() {
		return this.properties;
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
	 * Returns a string representation of this object displaying its concrete type and its values
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "UserRep [userId=" + this.userId + ", username=" + this.username + ", firstname=" + this.firstname +
				", lastname=" + this.lastname + ", userState=" + this.userState + ", locale=" + this.locale +
				", roles=" + this.roles + "]";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.username == null) ? 0 : this.username.hashCode());
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
		UserRep other = (UserRep) obj;
		if (this.username == null) {
			return other.username == null;
		} else
			return this.username.equals(other.username);
	}

	public UserRep getCopy() {
		return new UserRep(this.userId, this.username, this.firstname, this.lastname, this.userState, this.groups,
				this.roles, this.locale, this.properties, this.history);
	}

	public <T> T accept(PrivilegeElementVisitor<T> visitor) {
		return visitor.visitUserRep(this);
	}
}
