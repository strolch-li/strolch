/*
 * Copyright (c) 2010 - 2012
 * 
 * This file is part of Privilege.
 *
 * Privilege is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Privilege is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Privilege.  If not, see <http://www.gnu.org/licenses/>.
 * 
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
 * 
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

		if (userId == null || userId.isEmpty()) {
			throw new PrivilegeException("No UserId defined!");
		}
		if (username == null || username.isEmpty()) {
			throw new PrivilegeException("No username defined!");
		}
		if (firstname == null || firstname.isEmpty()) {
			throw new PrivilegeException("No firstname defined!");
		}
		if (surname == null || surname.isEmpty()) {
			throw new PrivilegeException("No surname defined!");
		}
		if (userState == null) {
			throw new PrivilegeException("No userState defined!");
		}

		// password may be null, meaning not able to login
		// roles may be null, meaning not able to login and must be added later
		// locale may be null, meaning use system default
		// properties may be null, meaning no properties

		this.userId = userId;

		this.username = username;
		this.password = password;
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
	 * @see java.lang.Object#toString()
	 */
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

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.userId == null) ? 0 : this.userId.hashCode());
		return result;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
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
