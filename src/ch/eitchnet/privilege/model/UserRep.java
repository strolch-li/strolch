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
import java.util.Set;

/**
 * @author rvonburg
 * 
 */
public class UserRep implements Serializable {

	private static final long serialVersionUID = 1L;

	public final String username;
	public final String firstname;
	public final String surname;
	public final UserState userState;
	public final Set<String> roles;
	public final Locale locale;

	/**
	 * @param username
	 * @param firstname
	 * @param surname
	 * @param userState
	 * @param roles
	 * @param locale
	 */
	public UserRep(String username, String firstname, String surname, UserState userState, Set<String> roles,
			Locale locale) {
		this.username = username;
		this.firstname = firstname;
		this.surname = surname;
		this.userState = userState;
		this.roles = roles;
		this.locale = locale;
	}
}
