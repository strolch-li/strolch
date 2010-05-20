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

import java.util.List;

/**
 * @author rvonburg
 * 
 */
public class User {

	private final String sessionId;
	private final String authToken;
	private final String authPassword;

	private final String username;
	private final String firstname;
	private final String surname;

	private final UserState userState;

	private List<String> roleList;

	/**
	 * The {@link User} constructor is private to ensure no unauthorized creation of {@link User} objects
	 * 
	 * @param sessionId
	 * @param username
	 * @param firstname
	 * @param surname
	 * @param userState
	 * @param validated
	 * @param roleList
	 */
	private User(String sessionId, String authToken, String authPassword, String username, String firstname,
			String surname, UserState userState, List<String> roleList) {

		this.sessionId = sessionId;
		this.authToken = authToken;
		this.authPassword = authPassword;

		this.username = username;
		this.userState = userState;

		this.firstname = firstname;
		this.surname = surname;

		this.roleList = roleList;
	}

	/**
	 * @return the sessionId
	 */
	public String getSessionId() {
		return sessionId;
	}

	/**
	 * @return the authToken
	 */
	public String getAuthToken() {
		return authToken;
	}

	/**
	 * @return the authPassword
	 */
	public String getAuthPassword() {
		return authPassword;
	}

	/**
	 * @return the username
	 */
	public String getUsername() {
		return username;
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
	public UserState getUserState() {
		return userState;
	}

	/**
	 * @return the roleList
	 */
	public List<String> getRoleList() {
		return roleList;
	}

	/**
	 * @param sessionId
	 * @param authToken
	 * @param authPassword
	 * @param username
	 * @param firstname
	 * @param surname
	 * @param userState
	 * @param roleList
	 * 
	 * @return a new {@link User} object which is authenticated on the current Java Virtual Machine
	 */
	public static User buildUser(String sessionId, String authToken, String authPassword, String username,
			String firstname, String surname, UserState userState, List<String> roleList) {

		// TODO validate who is creating this User object

		User user = new User(sessionId, authToken, authPassword, username, firstname, surname, userState, roleList);

		return user;
	}
}
