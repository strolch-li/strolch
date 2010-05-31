/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.model;

import java.io.Serializable;
import java.util.Locale;

import ch.eitchnet.privilege.i18n.PrivilegeException;

/**
 * @author rvonburg
 * 
 */
public final class Certificate implements Serializable {

	private static final long serialVersionUID = 1L;

	private final String sessionId;
	private final String username;
	private final String authToken;
	private final String authPassword;

	private Locale locale;

	/**
	 * @param sessionId
	 * @param username
	 * @param authToken
	 * @param authPassword
	 * @param locale
	 */
	public Certificate(String sessionId, String username, String authToken, String authPassword, Locale locale) {

		// validate arguments are not null
		if (sessionId == null || username == null || authToken == null || authPassword == null) {
			throw new PrivilegeException("One of the arguments is null!");
		}

		this.sessionId = sessionId;
		this.username = username;
		this.authToken = authToken;
		this.authPassword = authPassword;

		// if no locale is given, set default
		if (locale == null)
			this.locale = Locale.getDefault();
		else
			this.locale = locale;
	}

	/**
	 * @return the locale
	 */
	public Locale getLocale() {
		return locale;
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
		return sessionId;
	}

	/**
	 * @return the username
	 */
	public String getUsername() {
		return username;
	}

	/**
	 * Returns the authToken if the given authPassword is corret, null otherwise
	 * 
	 * @param authPassword
	 *            the auth password with which this certificate was created
	 * 
	 * @return the authToken if the given authPassword is corret, null otherwise
	 */
	public String getAuthToken(String authPassword) {
		if (this.authPassword.equals(authPassword)) {
			return authToken;
		} else {
			return null;
		}
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((authPassword == null) ? 0 : authPassword.hashCode());
		result = prime * result + ((authToken == null) ? 0 : authToken.hashCode());
		result = prime * result + ((locale == null) ? 0 : locale.hashCode());
		result = prime * result + ((sessionId == null) ? 0 : sessionId.hashCode());
		result = prime * result + ((username == null) ? 0 : username.hashCode());
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
		if (!(obj instanceof Certificate))
			return false;
		Certificate other = (Certificate) obj;
		if (authPassword == null) {
			if (other.authPassword != null)
				return false;
		} else if (!authPassword.equals(other.authPassword))
			return false;
		if (authToken == null) {
			if (other.authToken != null)
				return false;
		} else if (!authToken.equals(other.authToken))
			return false;
		if (locale == null) {
			if (other.locale != null)
				return false;
		} else if (!locale.equals(other.locale))
			return false;
		if (sessionId == null) {
			if (other.sessionId != null)
				return false;
		} else if (!sessionId.equals(other.sessionId))
			return false;
		if (username == null) {
			if (other.username != null)
				return false;
		} else if (!username.equals(other.username))
			return false;
		return true;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "Certificate [sessionId=" + sessionId + ", username=" + username + ", locale=" + locale + "]";
	}
}
