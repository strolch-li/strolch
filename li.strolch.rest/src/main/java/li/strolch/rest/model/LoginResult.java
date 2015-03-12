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
package li.strolch.rest.model;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import ch.eitchnet.utils.xml.XmlKeyValue;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "LoginResult")
public class LoginResult {

	@XmlAttribute(name = "username")
	private String username;

	@XmlAttribute(name = "sessionId")
	private String sessionId;

	@XmlAttribute(name = "locale")
	private String locale;

	@XmlAttribute(name = "msg")
	private String msg;

	@XmlElement(name = "roles")
	private List<String> roles;

	@XmlElement(name = "privileges")
	private List<Privilege> privileges;

	private Map<String, String> parameters;

	public LoginResult() {
		// no-arg constructor for JAXB
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
	 * @return the sessionId
	 */
	public String getSessionId() {
		return this.sessionId;
	}

	/**
	 * @param sessionId
	 *            the sessionId to set
	 */
	public void setSessionId(String sessionId) {
		this.sessionId = sessionId;
	}

	/**
	 * @return the locale
	 */
	public String getLocale() {
		return this.locale;
	}

	/**
	 * @param locale
	 *            the locale to set
	 */
	public void setLocale(String locale) {
		this.locale = locale;
	}

	/**
	 * @param locale
	 *            the locale to set
	 */
	public void setLocale(Locale locale) {
		this.locale = locale.toString();
	}

	/**
	 * @return the parameters
	 */
	public Map<String, String> getParameters() {
		return this.parameters;
	}

	/**
	 * @param parameters
	 *            the parameters to set
	 */
	public void setParameters(Map<String, String> parameters) {
		this.parameters = parameters;
	}

	/**
	 * Returns the string map properties of this user as a list of {@link XmlKeyValue} elements
	 * 
	 * @return the string map properties of this user as a list of {@link XmlKeyValue} elements
	 */
	@XmlElement(name = "properties")
	public List<XmlKeyValue> getPropertiesAsKeyValue() {
		if (this.parameters == null)
			return new ArrayList<>(0);
		return XmlKeyValue.valueOf(this.parameters);
	}

	/**
	 * @return the msg
	 */
	public String getMsg() {
		return this.msg;
	}

	/**
	 * @param msg
	 *            the msg to set
	 */
	public void setMsg(String msg) {
		this.msg = msg;
	}

	/**
	 * @return the roles
	 */
	public List<String> getRoles() {
		return roles;
	}

	/**
	 * @param roles
	 *            the roles to set
	 */
	public void setRoles(List<String> roles) {
		this.roles = roles;
	}

	/**
	 * @return the privileges
	 */
	public List<Privilege> getPrivileges() {
		return this.privileges;
	}

	/**
	 * @param privileges
	 *            the privileges to set
	 */
	public void setPrivileges(List<Privilege> privileges) {
		this.privileges = privileges;
	}

	@XmlRootElement(name = "Privilege")
	@XmlAccessorType(XmlAccessType.NONE)
	public static class Privilege {

		@XmlAttribute(name = "name")
		private String name;
		@XmlAttribute(name = "allAllowed")
		private boolean allAllowed;
		@XmlElement(name = "allowList")
		private List<String> allowList;

		public Privilege() {
			// no-arg constructor for JAXB
		}

		public Privilege(String name, boolean allAllowed, List<String> allowList) {
			this.name = name;
			this.allAllowed = allAllowed;
			this.allowList = allowList;
		}

		public void setName(String name) {
			this.name = name;
		}

		public String getName() {
			return this.name;
		}

		public List<String> getAllowList() {
			return this.allowList;
		}

		public void setAllowList(List<String> allowList) {
			this.allowList = allowList;
		}

		public boolean isAllAllowed() {
			return this.allAllowed;
		}

		public void setAllAllowed(boolean allAllowed) {
			this.allAllowed = allAllowed;
		}

	}
}
