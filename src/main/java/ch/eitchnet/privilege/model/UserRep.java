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
import java.util.Set;

import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.User;

/**
 * To keep certain details of the {@link User} itself hidden from remote clients and make sure instances are only edited
 * by users with the correct privilege, this representational version is allowed to be viewed by remote clients and
 * simply wraps all public data from the {@link User}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
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
	private Map<String, String> propertyMap;

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
	 * @param propertyMap
	 *            a {@link Map} containing string value pairs of properties for this user
	 */
	public UserRep(String userId, String username, String firstname, String surname, UserState userState,
			Set<String> roles, Locale locale, Map<String, String> propertyMap) {
		this.userId = userId;
		this.username = username;
		this.firstname = firstname;
		this.surname = surname;
		this.userState = userState;
		this.roles = roles;
		this.locale = locale;
		this.propertyMap = propertyMap;
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

	/**
	 * Returns the property with the given key
	 * 
	 * @param key
	 *            the key for which the property is to be returned
	 * 
	 * @return the property with the given key, or null if the property is not defined
	 */
	public String getProperty(String key) {
		return this.propertyMap.get(key);
	}

	/**
	 * Set the property with the key to the value
	 * 
	 * @param key
	 *            the key of the property to set
	 * @param value
	 *            the value of the property to set
	 */
	public void setProperty(String key, String value) {
		this.propertyMap.put(key, value);
	}

	/**
	 * Returns the {@link Set} of keys of all properties
	 * 
	 * @return the {@link Set} of keys of all properties
	 */
	public Set<String> getPropertyKeySet() {
		return this.propertyMap.keySet();
	}

	/**
	 * Returns the map of properties
	 * 
	 * @return the map of properties
	 */
	public Map<String, String> getProperties() {
		return this.propertyMap;
	}
}
