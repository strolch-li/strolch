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

import java.io.Serializable;
import java.util.*;

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.model.internal.User;
import li.strolch.utils.helper.StringHelper;

/**
 * The {@link Certificate} is the object a client keeps when accessing a Privilege enabled system. This object is the
 * instance which is always used when performing an access and is returned when a user performs a login through {@link
 * PrivilegeHandler#authenticate(String, byte[])}
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public final class Certificate implements Serializable {

	private final Usage usage;
	private final String sessionId;
	private final String username;
	private final String firstname;
	private final String lastname;
	private final UserState userState;
	private final String authToken;
	private final Date loginTime;

	private final Set<String> userRoles;
	private final Map<String, String> propertyMap;

	private Locale locale;
	private Date lastAccess;

	/**
	 * Default constructor initializing with all information needed for this certificate
	 *
	 * <p>
	 * Note, both the authentication token and password are private fields which are generated on login and only known
	 * by the {@link PrivilegeHandler}
	 * </p>
	 *
	 * @param usage
	 * 		the usage allowed for this certificate
	 * @param sessionId
	 * 		the users session id
	 * @param username
	 * 		the users login name
	 * @param firstname
	 * 		the users first name
	 * @param lastname
	 * 		the users last name
	 * @param authToken
	 * 		the authentication token defining the users unique session and is a private field of this certificate.
	 * @param locale
	 * 		the users {@link Locale}
	 * @param userRoles
	 * 		the user's roles
	 * @param propertyMap
	 * 		a {@link Map} containing string value pairs of properties for the logged in user. These properties can be
	 * 		edited and can be used for the user to change settings of this session
	 */
	public Certificate(Usage usage, String sessionId, String username, String firstname, String lastname,
			UserState userState, String authToken, Date loginTime, Locale locale, Set<String> userRoles,
			Map<String, String> propertyMap) {

		// validate arguments are not null
		if (StringHelper.isEmpty(sessionId)) {
			throw new PrivilegeException("sessionId is null!"); //$NON-NLS-1$
		}
		if (StringHelper.isEmpty(username)) {
			throw new PrivilegeException("username is null!"); //$NON-NLS-1$
		}
		if (StringHelper.isEmpty(authToken)) {
			throw new PrivilegeException("authToken is null!"); //$NON-NLS-1$
		}
		if (userState == null) {
			throw new PrivilegeException("userState is null!"); //$NON-NLS-1$
		}
		if (usage == null) {
			throw new PrivilegeException("usage is null!"); //$NON-NLS-1$
		}

		this.usage = usage;
		this.sessionId = sessionId;
		this.username = username;
		this.firstname = firstname;
		this.lastname = lastname;
		this.userState = userState;
		this.authToken = authToken;
		this.loginTime = loginTime;

		// if no locale is given, set default
		if (locale == null)
			this.locale = Locale.getDefault();
		else
			this.locale = locale;

		if (propertyMap == null)
			this.propertyMap = Collections.emptyMap();
		else
			this.propertyMap = Collections.unmodifiableMap(propertyMap);

		this.userRoles = Collections.unmodifiableSet(userRoles);
	}

	/**
	 * Returns the {@link Usage}
	 *
	 * @return the {@link Usage}
	 */
	public Usage getUsage() {
		return this.usage;
	}

	/**
	 * Returns the set or roles this user has
	 *
	 * @return the user's roles
	 */
	public Set<String> getUserRoles() {
		return this.userRoles;
	}

	/**
	 * Returns true if the user of this certificate has the given role
	 *
	 * @param role
	 * 		the role to check for
	 *
	 * @return true if the user of this certificate has the given role
	 */
	public boolean hasRole(String role) {
		return this.userRoles.contains(role);
	}

	/**
	 * Returns the {@link User User's} property map. The map is immutable
	 *
	 * @return the propertyMap
	 */
	public Map<String, String> getPropertyMap() {
		return this.propertyMap;
	}

	/**
	 * Returns the property with the given key
	 *
	 * @param key
	 * 		the key for which the property is to be returned
	 *
	 * @return the value of the property with the given key, or null if it does not exist
	 */
	public String getProperty(String key) {
		return this.propertyMap.get(key);
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
	 * @return the sessionId
	 */
	public String getSessionId() {
		return this.sessionId;
	}

	/**
	 * @return the username
	 */
	public String getUsername() {
		return this.username;
	}

	/**
	 * @return the firstname
	 */
	public String getFirstname() {
		return this.firstname;
	}

	/**
	 * @return the lastname
	 */
	public String getLastname() {
		return this.lastname;
	}

	/**
	 * @return the userState
	 */
	public UserState getUserState() {
		return userState;
	}

	/**
	 * @return the loginTime
	 */
	public Date getLoginTime() {
		return this.loginTime;
	}

	/**
	 * Returns the authToken if the given authPassword is correct, null otherwise
	 *
	 * @return the authToken if the given authPassword is correct, null otherwise
	 */
	public String getAuthToken() {
		return this.authToken;
	}

	/**
	 * @return the lastAccess
	 */
	public Date getLastAccess() {
		return this.lastAccess;
	}

	/**
	 * @param lastAccess
	 * 		the lastAccess to set
	 */
	public void setLastAccess(Date lastAccess) {
		this.lastAccess = lastAccess;
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
		builder.append("Certificate [sessionId=");
		builder.append(this.sessionId);
		builder.append(", username=");
		builder.append(this.username);

		if (StringHelper.isNotEmpty(this.firstname)) {
			builder.append(", firstname=");
			builder.append(this.firstname);
		}

		if (StringHelper.isNotEmpty(this.lastname)) {
			builder.append(", lastname=");
			builder.append(this.lastname);
		}

		builder.append(", locale=");
		builder.append(this.locale);

		builder.append("]");
		return builder.toString();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.authToken == null) ? 0 : this.authToken.hashCode());
		result = prime * result + ((this.locale == null) ? 0 : this.locale.hashCode());
		result = prime * result + ((this.sessionId == null) ? 0 : this.sessionId.hashCode());
		result = prime * result + ((this.username == null) ? 0 : this.username.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof Certificate))
			return false;
		Certificate other = (Certificate) obj;
		if (this.authToken == null) {
			if (other.authToken != null)
				return false;
		} else if (!this.authToken.equals(other.authToken))
			return false;
		if (this.locale == null) {
			if (other.locale != null)
				return false;
		} else if (!this.locale.equals(other.locale))
			return false;
		if (this.sessionId == null) {
			if (other.sessionId != null)
				return false;
		} else if (!this.sessionId.equals(other.sessionId))
			return false;
		if (this.username == null) {
			if (other.username != null)
				return false;
		} else if (!this.username.equals(other.username))
			return false;
		return true;
	}
}
