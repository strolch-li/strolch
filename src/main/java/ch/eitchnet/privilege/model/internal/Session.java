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

import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.model.Certificate;

/**
 * <p>
 * A {@link Session} is linked to currently logged in user. The {@link PrivilegeHandler} creates an instance of this
 * class once a {@link User} has successfully logged in and keeps this object private but hands out a
 * {@link Certificate} which the user must use every time a privilege needs to be granted
 * </p>
 * 
 * <p>
 * Note: This is an internal object which is not to be serialized or passed to clients, the client must keep his
 * {@link Certificate} which is used for accessing privileges
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public final class Session {

	private final String sessionId;
	private final String username;
	private final long loginTime;

	private final String authToken;
	private final String authPassword;

	/**
	 * Default constructor
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
	 *            the authentication token defining the users unique session and is a private field of this session. It
	 *            corresponds with the authentication token on the {@link Certificate}
	 * @param authPassword
	 *            the password to access the authentication token, this is not known to the client but set by the
	 *            {@link PrivilegeHandler} on authentication. It corresponds with the authentication password on the
	 *            {@link Certificate}
	 * @param loginTime
	 *            the time the user logged in
	 */
	public Session(String sessionId, String username, String authToken, String authPassword, long loginTime) {

		if (sessionId == null || sessionId.isEmpty()) {
			throw new PrivilegeException("No sessionId defined!");
		}
		if (username == null || username.isEmpty()) {
			throw new PrivilegeException("No username defined!");
		}
		if (authToken == null || authToken.isEmpty()) {
			throw new PrivilegeException("No authToken defined!");
		}
		if (authPassword == null || authPassword.isEmpty()) {
			throw new PrivilegeException("No authPassword defined!");
		}

		this.sessionId = sessionId;
		this.username = username;
		this.authToken = authToken;
		this.authPassword = authPassword;
		this.loginTime = loginTime;
	}

	/**
	 * @return the sessionId
	 */
	public String getSessionId() {
		return this.sessionId;
	}

	/**
	 * @return the authToken
	 */
	public String getAuthToken() {
		return this.authToken;
	}

	/**
	 * @return the authPassword
	 */
	public String getAuthPassword() {
		return this.authPassword;
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
	 * Returns a string representation of this object displaying its concrete type and its values
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("Session [sessionId=");
		builder.append(this.sessionId);
		builder.append(", username=");
		builder.append(this.username);
		builder.append(", loginTime=");
		builder.append(this.loginTime);
		builder.append("]");
		return builder.toString();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.authPassword == null) ? 0 : this.authPassword.hashCode());
		result = prime * result + ((this.authToken == null) ? 0 : this.authToken.hashCode());
		result = prime * result + (int) (this.loginTime ^ (this.loginTime >>> 32));
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
		if (getClass() != obj.getClass())
			return false;
		Session other = (Session) obj;
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
		if (this.loginTime != other.loginTime)
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
