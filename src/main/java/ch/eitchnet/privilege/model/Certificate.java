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
package ch.eitchnet.privilege.model;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.model.internal.User;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * The {@link Certificate} is the object a client keeps when accessing a Privilege enabled system. This object is the
 * instance which is always used when performing an access and is returned when a user performs a login through
 * {@link PrivilegeHandler#authenticate(String, byte[])}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public final class Certificate implements Serializable {

	private static final long serialVersionUID = 1L;

	private final String sessionId;
	private final long loginTime;
	private final String username;
	private final String firstname;
	private final String lastname;
	private final String authToken;

	private Locale locale;

	private Map<String, String> propertyMap;
	private Map<String, String> sessionDataMap;

	/**
	 * Default constructor initializing with all information needed for this certificate
	 * 
	 * <p>
	 * Note, both the authentication token and password are private fields which are generated on login and only known
	 * by the {@link PrivilegeHandler}
	 * </p>
	 * 
	 * @param sessionId
	 *            the users session id
	 * @param username
	 *            the users login name
	 * @param firstname
	 *            the users first name
	 * @param lastname
	 *            the users last name
	 * @param authToken
	 *            the authentication token defining the users unique session and is a private field of this certificate.
	 * @param locale
	 *            the users {@link Locale}
	 * @param propertyMap
	 *            a {@link Map} containing string value pairs of properties for the logged in user. These properties can
	 *            be edited and can be used for the user to change settings of this session
	 */
	public Certificate(String sessionId, long loginTime, String username, String firstname, String lastname,
			String authToken, Locale locale, Map<String, String> propertyMap) {

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

		this.sessionId = sessionId;
		this.loginTime = loginTime;
		this.username = username;
		this.firstname = firstname;
		this.lastname = lastname;
		this.authToken = authToken;

		// if no locale is given, set default
		if (locale == null)
			this.locale = Locale.getDefault();
		else
			this.locale = locale;

		if (propertyMap == null)
			this.propertyMap = Collections.emptyMap();
		else
			this.propertyMap = Collections.unmodifiableMap(propertyMap);

		this.sessionDataMap = new HashMap<>();
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
	 * Returns a mutable {@link Map} for storing session relevant data
	 * 
	 * @return the sessionDataMap
	 */
	public Map<String, String> getSessionDataMap() {
		return this.sessionDataMap;
	}

	/**
	 * @return the locale
	 */
	public Locale getLocale() {
		return this.locale;
	}

	/**
	 * @param locale
	 *            the locale to set
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
	 * @return the loginTime
	 */
	public long getLoginTime() {
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
