/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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

import java.util.Date;
import java.util.Locale;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import ch.eitchnet.privilege.model.Certificate;

@XmlRootElement(name = "UserSession")
@XmlAccessorType(XmlAccessType.NONE)
public class UserSession {

	@XmlAttribute(name = "sessionId")
	private String sessionId;
	@XmlAttribute(name = "loginTime")
	private Date loginTime;
	@XmlAttribute(name = "username")
	private String username;
	@XmlAttribute(name = "firstname")
	private String firstname;
	@XmlAttribute(name = "lastname")
	private String lastname;
	@XmlElement(name = "roles")
	private Set<String> userRoles;
	@XmlAttribute(name = "locale")
	private Locale locale;
	@XmlAttribute(name = "lastAccess")
	private Date lastAccess;

	public UserSession() {
		// no-arg constructor for JAXB
	}

	public UserSession(Certificate certificate) {
		this.sessionId = certificate.getSessionId();
		this.loginTime = certificate.getLoginTime();
		this.username = certificate.getUsername();
		this.firstname = certificate.getFirstname();
		this.lastname = certificate.getLastname();
		this.userRoles = certificate.getUserRoles();
		this.locale = certificate.getLocale();
		this.lastAccess = certificate.getLastAccess();
	}

	public Locale getLocale() {
		return locale;
	}

	public Date getLastAccess() {
		return lastAccess;
	}

	public String getSessionId() {
		return sessionId;
	}

	public Date getLoginTime() {
		return loginTime;
	}

	public String getUsername() {
		return username;
	}

	public String getFirstname() {
		return firstname;
	}

	public String getLastname() {
		return lastname;
	}

	public Set<String> getUserRoles() {
		return userRoles;
	}
}
