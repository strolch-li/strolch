/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.model.internal;

import java.util.Collections;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

import ch.eitchnet.privilege.model.UserRep;
import ch.eitchnet.privilege.model.UserState;

/**
 * @author rvonburg
 * 
 */
public final class User {

	private final String username;
	private final String password;

	private final String firstname;
	private final String surname;

	private final UserState userState;

	private final Set<String> roles;

	private final Locale locale;

	/**
	 * 
	 * @param username
	 * @param password
	 * @param firstname
	 * @param surname
	 * @param userState
	 * @param roles
	 * @param locale
	 */
	public User(String username, String password, String firstname, String surname, UserState userState,
			Set<String> roles, Locale locale) {

		this.username = username;
		this.password = password;
		this.userState = userState;

		this.firstname = firstname;
		this.surname = surname;

		this.roles = Collections.unmodifiableSet(roles);

		this.locale = locale;
	}

	/**
	 * @return the username
	 */
	public String getUsername() {
		return username;
	}

	/**
	 * 
	 * @param privilegeHandler
	 * @param certificate
	 * 
	 * @return
	 */
	public String getPassword() {

		// TODO is it possible that there is a hidden way of accessing this 
		// field even though? The User object should be private, but maybe I 
		// forgot something?

		return password;
	}

	/**
	 * @return the password
	 */
	public boolean isPassword(String password) {
		return this.password.equals(password);
	}

	/**
	 * @return the firstname
	 */
	public String getFirstname() {
		return firstname;
	}

	/**
	 * @return the surname
	 */
	public String getSurname() {
		return surname;
	}

	/**
	 * @return the userState
	 */
	public UserState getState() {
		return userState;
	}

	/**
	 * @return the roles
	 */
	public Set<String> getRoles() {
		return roles;
	}

	/**
	 * Returns true if this user has the specified role
	 * 
	 * @param role
	 * @return true if the this user has the specified role
	 */
	public boolean hasRole(String role) {
		return roles.contains(role);
	}

	/**
	 * @return the locale
	 */
	public Locale getLocale() {
		return locale;
	}

	/**
	 * @return a {@link UserRep} which is a representation of this object used to serialize and view on clients
	 */
	public UserRep asUserRep() {
		return new UserRep(username, firstname, surname, userState, new HashSet<String>(roles), locale);
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("User [username=");
		builder.append(username);
		builder.append(", firstname=");
		builder.append(firstname);
		builder.append(", surname=");
		builder.append(surname);
		builder.append(", locale=");
		builder.append(locale);
		builder.append(", userState=");
		builder.append(userState);
		builder.append(", roles=");
		builder.append(roles);
		builder.append("]");
		return builder.toString();
	}
}
