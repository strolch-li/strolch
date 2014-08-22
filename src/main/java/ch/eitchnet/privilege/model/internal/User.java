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
package ch.eitchnet.privilege.model.internal;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.model.UserRep;
import ch.eitchnet.privilege.model.UserState;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * This class defines the actual login information for a given user which can be granted privileges. Every user is
 * granted a set of {@link Role}s and has a {@link UserState} including detail information like first name and surname
 * 
 * <p>
 * Note: This is an internal object which is not to be serialized or passed to clients, {@link UserRep}s are used for
 * that
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public final class User {

	private final String userId;

	private final String username;
	private final String password;

	private final String firstname;
	private final String surname;

	private final UserState userState;

	private final Set<String> roles;

	private final Map<String, String> propertyMap;

	private final Locale locale;

	/**
	 * Default constructor
	 * 
	 * @param userId
	 *            the user's id
	 * @param username
	 *            the user's login name
	 * @param password
	 *            the user's password (hashed)
	 * @param firstname
	 *            the user's first name
	 * @param surname
	 *            the user's surname
	 * @param userState
	 *            the user's {@link UserState}
	 * @param roles
	 *            the set of {@link Role}s assigned to this user
	 * @param locale
	 *            the user's {@link Locale}
	 * @param propertyMap
	 *            a {@link Map} containing string value pairs of properties for this user
	 */
	public User(String userId, String username, String password, String firstname, String surname, UserState userState,
			Set<String> roles, Locale locale, Map<String, String> propertyMap) {

		if (StringHelper.isEmpty(userId)) {
			throw new PrivilegeException("No UserId defined!"); //$NON-NLS-1$
		}
		if (userState == null) {
			throw new PrivilegeException("No userState defined!"); //$NON-NLS-1$
		}
		if (StringHelper.isEmpty(username)) {
			throw new PrivilegeException("No username defined!"); //$NON-NLS-1$
		}
		if (userState != UserState.SYSTEM) {
			if (StringHelper.isEmpty(surname)) {
				throw new PrivilegeException("No surname defined!"); //$NON-NLS-1$
			}
			if (StringHelper.isEmpty(firstname)) {
				throw new PrivilegeException("No firstname defined!"); //$NON-NLS-1$
			}
		}

		// password may be null, meaning not able to login
		// roles may be null, meaning not able to login and must be added later
		// locale may be null, meaning use system default
		// properties may be null, meaning no properties

		this.userId = userId;

		this.username = username;
		this.password = StringHelper.isEmpty(password) ? null : password;
		this.userState = userState;

		this.firstname = firstname;
		this.surname = surname;

		if (roles == null)
			this.roles = Collections.emptySet();
		else
			this.roles = Collections.unmodifiableSet(new HashSet<String>(roles));

		if (locale == null)
			this.locale = Locale.getDefault();
		else
			this.locale = locale;

		if (propertyMap == null)
			this.propertyMap = Collections.emptyMap();
		else
			this.propertyMap = Collections.unmodifiableMap(new HashMap<String, String>(propertyMap));
	}

	/**
	 * @return the userId
	 */
	public String getUserId() {
		return this.userId;
	}

	/**
	 * @return the username
	 */
	public String getUsername() {
		return this.username;
	}

	/**
	 * Returns the hashed password for this {@link User}
	 * 
	 * @return the hashed password for this {@link User}
	 */
	public String getPassword() {
		return this.password;
	}

	/**
	 * @return the firstname
	 */
	public String getFirstname() {
		return this.firstname;
	}

	/**
	 * @return the surname
	 */
	public String getSurname() {
		return this.surname;
	}

	/**
	 * @return the userState
	 */
	public UserState getUserState() {
		return this.userState;
	}

	/**
	 * @return the roles
	 */
	public Set<String> getRoles() {
		return this.roles;
	}

	/**
	 * Returns true if this user has the specified role
	 * 
	 * @param role
	 *            the name of the {@link Role} to check for
	 * 
	 * @return true if the this user has the specified role
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
	 * Returns the property with the given key
	 * 
	 * @param key
	 *            the key for which the property is to be returned
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
	 * @return a {@link UserRep} which is a representation of this object used to serialize and view on clients
	 */
	public UserRep asUserRep() {
		return new UserRep(this.userId, this.username, this.firstname, this.surname, this.userState,
				new HashSet<String>(this.roles), this.locale, new HashMap<String, String>(this.propertyMap));
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
		builder.append("User [userId=");
		builder.append(this.userId);
		builder.append(", username=");
		builder.append(this.username);
		builder.append(", firstname=");
		builder.append(this.firstname);
		builder.append(", surname=");
		builder.append(this.surname);
		builder.append(", locale=");
		builder.append(this.locale);
		builder.append(", userState=");
		builder.append(this.userState);
		builder.append(", roles=");
		builder.append(this.roles);
		builder.append("]");
		return builder.toString();
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
		if (this.userId == null) {
			if (other.userId != null)
				return false;
		} else if (!this.userId.equals(other.userId))
			return false;
		return true;
	}
}
