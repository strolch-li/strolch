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
import java.util.List;
import java.util.Locale;

import ch.eitchnet.privilege.i18n.PrivilegeException;
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

	private final List<String> roleList;

	private final Locale locale;

	/**
	 * The {@link User} constructor is private to ensure no unauthorized creation of {@link User} objects
	 */
	private User(String username, String password, String firstname, String surname, UserState userState,
			List<String> roleList, Locale locale) {

		this.username = username;
		this.password = password;
		this.userState = userState;

		this.firstname = firstname;
		this.surname = surname;

		this.roleList = roleList;

		this.locale = locale;
	}

	/**
	 * @return the username
	 */
	public String getUsername() {
		return username;
	}

	/**
	 * @return the password
	 */
	public String getPassword() {
		return password;
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
	 * @return the roleList
	 */
	public List<String> getRoleList() {
		return roleList;
	}

	/**
	 * @return the locale
	 */
	public Locale getLocale() {
		return locale;
	}

	/**
	 * @return a new {@link User} object which is authenticated on the current Java Virtual Machine
	 */
	public static User buildUser(String username, String password, String firstname, String surname,
			UserState userState, List<String> roleList, Locale locale) {

		// set a default locale
		if (locale == null)
			locale = Locale.getDefault();

		// TODO validate who is creating this User object
		
		if (username.length() < 3) {
			throw new PrivilegeException("The given username is shorter than 3 characters");
		}

		if (firstname.isEmpty()) {
			throw new PrivilegeException("The given firstname is empty");
		}

		if (surname.isEmpty()) {
			throw new PrivilegeException("The given firstname is empty");
		}

		User user = new User(username, password, firstname, surname, userState, Collections.unmodifiableList(roleList),
				locale);

		return user;
	}
}
