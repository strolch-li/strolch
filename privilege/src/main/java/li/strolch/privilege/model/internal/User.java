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

import static li.strolch.privilege.base.PrivilegeConstants.*;

import java.util.*;

import li.strolch.privilege.base.PrivilegeConstants;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.UserRep;
import li.strolch.privilege.model.UserState;
import li.strolch.utils.helper.StringHelper;

/**
 * This class defines the actual login information for a given user which can be granted privileges. Every user is
 * granted a set of {@link Role}s and has a {@link UserState} including detail information like first name and lastname
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
	private final byte[] password;
	private final byte[] salt;
	private final String hashAlgorithm;
	private final int hashIterations;
	private final int hashKeyLength;

	private final String firstname;
	private final String lastname;

	private final Set<String> roles;

	private final UserState userState;
	private final Map<String, String> propertyMap;
	private final Locale locale;

	private final boolean passwordChangeRequested;
	private final UserHistory history;

	/**
	 * Default constructor
	 *
	 * @param userId
	 * 		the user's id
	 * @param username
	 * 		the user's login name
	 * @param password
	 * 		the user's password (hashed)
	 * @param salt
	 * 		the password salt
	 * @param hashAlgorithm
	 * 		the algorithm for the hash
	 * @param hashIterations
	 * 		the nr of iterations for hashing
	 * @param hashKeyLength
	 * 		the hash key length
	 * @param firstname
	 * 		the user's first name
	 * @param lastname
	 * 		the user's lastname
	 * @param userState
	 * 		the user's {@link UserState}
	 * @param roles
	 * 		the set of {@link Role}s assigned to this user
	 * @param locale
	 * 		the user's {@link Locale}
	 * @param propertyMap
	 * 		a {@link Map} containing string value pairs of properties for this user
	 */
	public User(String userId, String username, byte[] password, byte[] salt, String hashAlgorithm, int hashIterations,
			int hashKeyLength, String firstname, String lastname, UserState userState, Set<String> roles, Locale locale,
			Map<String, String> propertyMap, boolean passwordChangeRequested, UserHistory history) {

		if (StringHelper.isEmpty(userId))
			throw new PrivilegeException("No UserId defined!"); //$NON-NLS-1$
		if (userState == null)
			throw new PrivilegeException("No userState defined!"); //$NON-NLS-1$
		if (StringHelper.isEmpty(username))
			throw new PrivilegeException("No username defined!"); //$NON-NLS-1$
		if (userState != UserState.SYSTEM) {
			if (StringHelper.isEmpty(lastname))
				throw new PrivilegeException("No lastname defined!"); //$NON-NLS-1$
			if (StringHelper.isEmpty(firstname))
				throw new PrivilegeException("No firstname defined!"); //$NON-NLS-1$
		}

		if (history == null)
			throw new PrivilegeException("History must not be null!");

		// password, salt and hash* may be null, meaning not able to login
		// roles may be null, meaning not able to login and must be added later
		// locale may be null, meaning use system default
		// properties may be null, meaning no properties

		this.userId = userId;

		this.username = username;
		this.password = password;
		this.salt = salt;

		this.hashAlgorithm = hashAlgorithm;
		this.hashIterations = hashIterations;
		this.hashKeyLength = hashKeyLength;

		this.userState = userState;

		this.firstname = firstname;
		this.lastname = lastname;

		if (roles == null)
			this.roles = Collections.emptySet();
		else
			this.roles = Set.copyOf(roles);

		if (locale == null)
			this.locale = Locale.getDefault();
		else
			this.locale = locale;

		if (propertyMap == null)
			this.propertyMap = Collections.emptyMap();
		else
			this.propertyMap = Map.copyOf(propertyMap);

		this.passwordChangeRequested = passwordChangeRequested;
		this.history = history;
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
	public byte[] getPassword() {
		return this.password;
	}

	/**
	 * Return the salt for this {@link User}
	 *
	 * @return the salt for this {@link User}
	 */
	public byte[] getSalt() {
		return this.salt;
	}

	/**
	 * Return the hash algorithm
	 *
	 * @return the hash algorithm
	 */
	public String getHashAlgorithm() {
		return this.hashAlgorithm;
	}

	/**
	 * Return the hashIterations
	 *
	 * @return hashIterations
	 */
	public int getHashIterations() {
		return this.hashIterations;
	}

	/**
	 * Return the hashKeyLength
	 *
	 * @return hashKeyLength
	 */
	public int getHashKeyLength() {
		return this.hashKeyLength;
	}

	/**
	 * @return the first name
	 */
	public String getFirstname() {
		return this.firstname;
	}

	/**
	 * @return the last name
	 */
	public String getLastname() {
		return this.lastname;
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
	 * 		the name of the {@link Role} to check for
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

	public boolean isPasswordChangeRequested() {
		return this.passwordChangeRequested;
	}

	/**
	 * Returns the History object
	 *
	 * @return the History object
	 */
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
	 * @param key
	 * 		the key for which the property is to be returned
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
		return new UserRep(this.userId, this.username, this.firstname, this.lastname, this.userState,
				new HashSet<>(this.roles), this.locale, new HashMap<>(this.propertyMap), this.history.getClone());
	}

	/**
	 * Returns a string representation of this object displaying its concrete type and its values
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		String builder = "User [userId=" + this.userId + ", username=" + this.username + ", firstname=" + this.firstname
				+ ", lastname=" + this.lastname + ", locale=" + this.locale + ", userState=" + this.userState
				+ ", roles=" + this.roles + "]";
		return builder;
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
			return other.userId == null;
		} else
			return this.userId.equals(other.userId);
	}
}
