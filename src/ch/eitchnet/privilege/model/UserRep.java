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

import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.User;

/**
 * To keep certain details of the {@link User} itself hidden from remote clients and make sure instances are only edited
 * by users with the correct privilege, this representational version is allowed to be viewed by remote clients and
 * simply wraps all public data from the {@link User}
 * 
 * @author rvonburg
 */
public class UserRep implements Serializable {

	private static final long serialVersionUID = 1L;

	private final String userId;
	private String username;
	private String firstname;
	private String surname;
	private UserState userState;
	private Set<String> roles;
	private Locale locale;

	/**
	 * Default constructor
	 * 
	 * @param userId
	 *            the user's id
	 * @param username
	 *            the user's login name
	 * @param firstname
	 *            the user's first name
	 * @param surname
	 *            the user's surname
	 * @param userState
	 *            the user's {@link UserState}
	 * @param roles
	 *            the set of {@link Role}s assigned to this user
	 * @param locale
	 *            the user's {@link Locale}
	 */
	public UserRep(String userId, String username, String firstname, String surname, UserState userState,
			Set<String> roles, Locale locale) {
		this.userId = userId;
		this.username = username;
		this.firstname = firstname;
		this.surname = surname;
		this.userState = userState;
		this.roles = roles;
		this.locale = locale;
	}

	/**
	 * @return the userId
	 */
	public String getUserId() {
		return this.userId;
	}

	/**
	 * @return the username
	 */
	public String getUsername() {
		return this.username;
	}

	/**
	 * @param username
	 *            the username to set
	 */
	public void setUsername(String username) {
		this.username = username;
	}

	/**
	 * @return the firstname
	 */
	public String getFirstname() {
		return this.firstname;
	}

	/**
	 * @param firstname
	 *            the firstname to set
	 */
	public void setFirstname(String firstname) {
		this.firstname = firstname;
	}

	/**
	 * @return the surname
	 */
	public String getSurname() {
		return this.surname;
	}

	/**
	 * @param surname
	 *            the surname to set
	 */
	public void setSurname(String surname) {
		this.surname = surname;
	}

	/**
	 * @return the userState
	 */
	public UserState getUserState() {
		return this.userState;
	}

	/**
	 * @param userState
	 *            the userState to set
	 */
	public void setUserState(UserState userState) {
		this.userState = userState;
	}

	/**
	 * @return the roles
	 */
	public Set<String> getRoles() {
		return this.roles;
	}

	/**
	 * @param roles
	 *            the roles to set
	 */
	public void setRoles(Set<String> roles) {
		this.roles = roles;
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
}
