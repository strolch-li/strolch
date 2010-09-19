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
		return this.username;
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
	public UserState getState() {
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
	 * @return a {@link UserRep} which is a representation of this object used to serialize and view on clients
	 */
	public UserRep asUserRep() {
		return new UserRep(this.username, this.firstname, this.surname, this.userState,
				new HashSet<String>(this.roles), this.locale);
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("User [username=");
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
}
