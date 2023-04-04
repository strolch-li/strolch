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

import static li.strolch.privilege.base.PrivilegeConstants.*;
import static li.strolch.utils.helper.StringHelper.trimOrEmpty;

import java.io.Serializable;
import java.text.MessageFormat;
import java.util.*;
import java.util.stream.Collectors;

import li.strolch.privilege.base.PrivilegeConstants;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserHistory;
import li.strolch.utils.helper.StringHelper;

/**
 * To keep certain details of the {@link User} itself hidden from remote clients and make sure instances are only edited
 * by users with the correct privilege, this representational version is allowed to be viewed by remote clients and
 * simply wraps all public data from the {@link User}
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class UserRep implements Serializable {

	private String userId;
	private String username;
	private String firstname;
	private String lastname;
	private UserState userState;
	private Locale locale;
	private Set<String> roles;
	private Map<String, String> properties;

	private UserHistory history;

	/**
	 * Default constructor
	 *
	 * @param userId
	 * 		the user's id
	 * @param username
	 * 		the user's login name
	 * @param firstname
	 * 		the user's first name
	 * @param lastname
	 * 		the user's last name
	 * @param userState
	 * 		the user's {@link UserState}
	 * @param roles
	 * 		the set of {@link Role}s assigned to this user
	 * @param locale
	 * 		the user's {@link Locale}
	 * @param propertyMap
	 * 		a {@link Map} containing string value pairs of properties for this user
	 */
	public UserRep(String userId, String username, String firstname, String lastname, UserState userState,
			Set<String> roles, Locale locale, Map<String, String> propertyMap, UserHistory history) {
		this.userId = trimOrEmpty(userId);
		this.username = trimOrEmpty(username);
		this.firstname = trimOrEmpty(firstname);
		this.lastname = trimOrEmpty(lastname);
		this.userState = userState;
		this.roles = roles == null ? null : roles.stream().map(String::trim).collect(Collectors.toSet());
		this.locale = locale;

		if (propertyMap != null) {
			this.properties = new HashMap<>();
			propertyMap.forEach((key, value) -> this.properties.put(key.trim(), value.trim()));
		}

		this.history = history;
	}

	/**
	 *
	 */
	@SuppressWarnings("unused")
	private UserRep() {
		// No arg constructor for JAXB
	}

	/**
	 * Validates that all required fields are set
	 */
	public void validate() {

		if (StringHelper.isEmpty(this.userId))
			throw new PrivilegeException("userId is null or empty"); //$NON-NLS-1$

		if (StringHelper.isEmpty(this.username))
			throw new PrivilegeException("username is null or empty"); //$NON-NLS-1$

		// username must be at least 2 characters in length
		if (this.username.length() < 2) {
			String msg = MessageFormat
					.format("The given username ''{0}'' is shorter than 2 characters", this.username); //$NON-NLS-1$
			throw new PrivilegeException(msg);
		}

		if (this.userState == null)
			throw new PrivilegeException("userState is null"); //$NON-NLS-1$

		if (StringHelper.isEmpty(this.firstname))
			throw new PrivilegeException("firstname is null or empty"); //$NON-NLS-1$

		if (StringHelper.isEmpty(this.lastname))
			throw new PrivilegeException("lastname is null or empty"); //$NON-NLS-1$

		if (this.roles == null || this.roles.isEmpty())
			throw new PrivilegeException("roles is null or empty"); //$NON-NLS-1$
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
	 * @param userId
	 * 		to set
	 */
	public void setUserId(String userId) {
		this.userId = trimOrEmpty(userId);
	}

	/**
	 * @return the username
	 */
	public String getUsername() {
		return this.username;
	}

	/**
	 * @param username
	 * 		the username to set
	 */
	public void setUsername(String username) {
		this.username = trimOrEmpty(username);
	}

	/**
	 * @return the firstname
	 */
	public String getFirstname() {
		return this.firstname;
	}

	/**
	 * @param firstname
	 * 		the firstname to set
	 */
	public void setFirstname(String firstname) {
		this.firstname = trimOrEmpty(firstname);
	}

	/**
	 * @return the lastname
	 */
	public String getLastname() {
		return this.lastname;
	}

	/**
	 * @param lastname
	 * 		the lastname to set
	 */
	public void setLastname(String lastname) {
		this.lastname = trimOrEmpty(lastname);
	}

	/**
	 * @return the userState
	 */
	public UserState getUserState() {
		return this.userState;
	}

	/**
	 * @param userState
	 * 		the userState to set
	 */
	public void setUserState(UserState userState) {
		this.userState = userState;
	}

	/**
	 * @return the roles
	 */
	public Set<String> getRoles() {
		return this.roles;
	}

	/**
	 * @param roles
	 * 		the roles to set
	 */
	public void setRoles(Set<String> roles) {
		this.roles = roles.stream().map(String::trim).collect(Collectors.toSet());
	}

	/**
	 * Returns true if this user has the given role
	 *
	 * @param role
	 * 		the role to check for
	 *
	 * @return returns true if this user has the given role
	 */
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
	 * @param locale
	 * 		the locale to set
	 */
	public void setLocale(Locale locale) {
		this.locale = locale;
	}

	/**
	 * Returns the {@link UserHistory}
	 *
	 * @return the user history
	 */
	public UserHistory getHistory() {
		if (this.history == null)
			return new UserHistory();
		return this.history;
	}

	/**
	 * Returns true if the the given property exists
	 *
	 * @param key
	 * 		the property key to check
	 *
	 * @return true if the the given property exists
	 */
	public boolean hasProperty(String key) {
		return this.properties.containsKey(key);
	}

	/**
	 * Returns the property with the given key
	 *
	 * @param key
	 * 		the key for which the property is to be returned
	 *
	 * @return the property with the given key, or null if the property is not defined
	 */
	public String getProperty(String key) {
		if (this.properties == null)
			return null;
		return this.properties.get(key);
	}

	/**
	 * Set the property with the key to the value
	 *
	 * @param key
	 * 		the key of the property to set
	 * @param value
	 * 		the value of the property to set
	 */
	public void setProperty(String key, String value) {
		if (this.properties == null)
			this.properties = new HashMap<>(1);
		this.properties.put(key.trim(), value.trim());
	}

	/**
	 * Returns the {@link Set} of keys of all properties
	 *
	 * @return the {@link Set} of keys of all properties
	 */
	public Set<String> getPropertyKeySet() {
		if (this.properties == null)
			return new HashSet<>();
		return new HashSet<>(this.properties.keySet());
	}

	/**
	 * Returns the map of properties
	 *
	 * @return the map of properties
	 */
	public Map<String, String> getProperties() {
		if (this.properties == null)
			return new HashMap<>();
		return new HashMap<>(this.properties);
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
	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("UserRep [userId=");
		builder.append(this.userId);
		builder.append(", username=");
		builder.append(this.username);
		builder.append(", firstname=");
		builder.append(this.firstname);
		builder.append(", lastname=");
		builder.append(this.lastname);
		builder.append(", userState=");
		builder.append(this.userState);
		builder.append(", locale=");
		builder.append(this.locale);
		builder.append(", roles=");
		builder.append(this.roles);
		builder.append("]");
		return builder.toString();
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
			if (other.username != null)
				return false;
		} else if (!this.username.equals(other.username))
			return false;
		return true;
	}

	@Override
	public UserRep clone() {

		Set<String> roles = new HashSet<>(this.roles);
		Map<String, String> propertyMap = this.properties == null ? null : new HashMap<>(this.properties);

		return new UserRep(this.userId, this.username, this.firstname, this.lastname, this.userState, roles,
				this.locale, propertyMap, this.history == null ? new UserHistory() : this.history.getClone());
	}

	public <T> T accept(PrivilegeElementVisitor<T> visitor) {
		return visitor.visitUserRep(this);
	}
}
