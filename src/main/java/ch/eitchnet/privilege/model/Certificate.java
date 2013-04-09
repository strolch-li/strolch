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
package ch.eitchnet.privilege.model;

import java.io.Serializable;
import java.util.Locale;
import java.util.Map;

import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * The {@link Certificate} is the object a client keeps when accessing a Privilege enabled system. This object is the
 * instance which is always used when performing an access and is returned when a user performs a login through
 * {@link PrivilegeHandler#authenticate(String, String)}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public final class Certificate implements Serializable {

	private static final long serialVersionUID = 1L;

	private final String sessionId;
	private final long loginTime;
	private final String username;
	private final String authToken;
	private final String authPassword;

	private Locale locale;

	private Map<String, String> propertyMap;

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
	 * @param authToken
	 *            the authentication token defining the users unique session and is a private field of this certificate.
	 *            It corresponds with the authentication token on the {@link Session}
	 * @param authPassword
	 *            the password to access the authentication token, this is not known to the client but set by the
	 *            {@link PrivilegeHandler} on authentication. It corresponds with the authentication password on the
	 *            {@link Session}
	 * @param locale
	 *            the users {@link Locale}
	 * @param propertyMap
	 *            a {@link Map} containing string value pairs of properties for the logged in user. These properties can
	 *            be edited and can be used for the user to change settings of this session
	 */
	public Certificate(String sessionId, long loginTime, String username, String authToken, String authPassword,
			Locale locale, Map<String, String> propertyMap) {

		// validate arguments are not null
		if (StringHelper.isEmpty(sessionId)) {
			throw new PrivilegeException("sessionId is null!");
		}
		if (StringHelper.isEmpty(username)) {
			throw new PrivilegeException("username is null!");
		}
		if (StringHelper.isEmpty(authToken)) {
			throw new PrivilegeException("authToken is null!");
		}
		if (StringHelper.isEmpty(authPassword)) {
			throw new PrivilegeException("authPassword is null!");
		}

		this.sessionId = sessionId;
		this.loginTime = loginTime;
		this.username = username;
		this.authToken = authToken;
		this.authPassword = authPassword;

		// if no locale is given, set default
		if (locale == null)
			this.locale = Locale.getDefault();
		else
			this.locale = locale;

		this.propertyMap = propertyMap;
	}

	/**
	 * @return the propertyMap
	 */
	public Map<String, String> getPropertyMap() {
		return this.propertyMap;
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
	 * @return the loginTime
	 */
	public long getLoginTime() {
		return this.loginTime;
	}

	/**
	 * Returns the authToken if the given authPassword is correct, null otherwise
	 * 
	 * @param authPassword
	 *            the authentication password with which this certificate was created
	 * 
	 * @return the authToken if the given authPassword is correct, null otherwise
	 */
	public String getAuthToken(String authPassword) {
		if (this.authPassword.equals(authPassword))
			return this.authToken;

		return null;
	}

	/**
	 * Returns a string representation of this object displaying its concrete type and its values
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("Certificate [sessionId=");
		builder.append(this.sessionId);
		builder.append(", username=");
		builder.append(this.username);
		builder.append(", locale=");
		builder.append(this.locale);
		builder.append("]");
		return builder.toString();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.authPassword == null) ? 0 : this.authPassword.hashCode());
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
		if (this.authPassword == null) {
			if (other.authPassword != null)
				return false;
		} else if (!this.authPassword.equals(other.authPassword))
			return false;
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
