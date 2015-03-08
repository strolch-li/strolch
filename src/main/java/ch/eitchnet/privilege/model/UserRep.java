/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ch.eitchnet.privilege.model;

import java.io.Serializable;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.User;
import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.utils.xml.XmlKeyValue;

/**
 * To keep certain details of the {@link User} itself hidden from remote clients and make sure instances are only edited
 * by users with the correct privilege, this representational version is allowed to be viewed by remote clients and
 * simply wraps all public data from the {@link User}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlRootElement(name = "User")
@XmlAccessorType(XmlAccessType.NONE)
public class UserRep implements Serializable {

	private static final long serialVersionUID = 1L;

	@XmlAttribute(name = "userId")
	private String userId;

	@XmlAttribute(name = "username")
	private String username;

	@XmlAttribute(name = "firstname")
	private String firstname;

	@XmlAttribute(name = "lastname")
	private String lastname;

	@XmlAttribute(name = "userState")
	private UserState userState;

	@XmlAttribute(name = "locale")
	private Locale locale;

	@XmlElement(name = "roles")
	private Set<String> roles;

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
	 * @param lastname
	 *            the user's last name
	 * @param userState
	 *            the user's {@link UserState}
	 * @param roles
	 *            the set of {@link Role}s assigned to this user
	 * @param locale
	 *            the user's {@link Locale}
	 * @param propertyMap
	 *            a {@link Map} containing string value pairs of properties for this user
	 */
	public UserRep(String userId, String username, String firstname, String lastname, UserState userState,
			Set<String> roles, Locale locale, Map<String, String> propertyMap) {
		this.userId = userId;
		this.username = username;
		this.firstname = firstname;
		this.lastname = lastname;
		this.userState = userState;
		this.roles = roles;
		this.locale = locale;
		this.propertyMap = propertyMap;
	}

	/**
	 * 
	 */
	@SuppressWarnings("unused")
	private UserRep() {
		// No arg constructor for JAXB
	}

	/**
	 * Validates that all required fields are set
	 */
	public void validate() {

		if (StringHelper.isEmpty(this.userId))
			throw new PrivilegeException("userId is null or empty"); //$NON-NLS-1$

		if (StringHelper.isEmpty(this.username))
			throw new PrivilegeException("username is null or empty"); //$NON-NLS-1$

		if (this.userState == null)
			throw new PrivilegeException("userState is null"); //$NON-NLS-1$

		if (StringHelper.isEmpty(this.firstname))
			throw new PrivilegeException("firstname is null or empty"); //$NON-NLS-1$

		if (StringHelper.isEmpty(this.lastname))
			throw new PrivilegeException("lastname is null or empty"); //$NON-NLS-1$

		if (this.roles == null || this.roles.isEmpty())
			throw new PrivilegeException("roles is null or empty"); //$NON-NLS-1$
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
	 * @return the lastname
	 */
	public String getLastname() {
		return this.lastname;
	}

	/**
	 * @param lastname
	 *            the lastname to set
	 */
	public void setLastname(String lastname) {
		this.lastname = lastname;
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

	/**
	 * Returns the string map properties of this user as a list of {@link XmlKeyValue} elements
	 * 
	 * @return the string map properties of this user as a list of {@link XmlKeyValue} elements
	 */
	@XmlElement(name = "properties")
	public List<XmlKeyValue> getPropertiesAsKeyValue() {
		return XmlKeyValue.valueOf(this.propertyMap);
	}

	/**
	 * Sets the string map properties of this user from the given list of {@link XmlKeyValue}
	 * 
	 * @param values
	 *            the list of {@link XmlKeyValue} from which to set the properties
	 */
	public void setPropertiesAsKeyValue(List<XmlKeyValue> values) {
		this.propertyMap = XmlKeyValue.toMap(values);
	}

	/**
	 * Returns a string representation of this object displaying its concrete type and its values
	 * 
	 * @see java.lang.Object#toString()
	 */
	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("UserRep [userId=");
		builder.append(this.userId);
		builder.append(", username=");
		builder.append(this.username);
		builder.append(", firstname=");
		builder.append(this.firstname);
		builder.append(", lastname=");
		builder.append(this.lastname);
		builder.append(", userState=");
		builder.append(this.userState);
		builder.append(", locale=");
		builder.append(this.locale);
		builder.append(", roles=");
		builder.append(this.roles);
		builder.append("]");
		return builder.toString();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
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
		UserRep other = (UserRep) obj;
		if (this.username == null) {
			if (other.username != null)
				return false;
		} else if (!this.username.equals(other.username))
			return false;
		return true;
	}
}
