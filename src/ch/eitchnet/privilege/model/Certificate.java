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

import ch.eitchnet.privilege.base.PrivilegeException;

/**
 * @author rvonburg
 * 
 */
public class Certificate implements Serializable {

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
}
