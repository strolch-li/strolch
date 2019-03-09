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

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import li.strolch.privilege.model.Certificate;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

public class UserSession {

	private String sessionId;
	private Date loginTime;
	private String username;
	private String firstname;
	private String lastname;
	private String source;
	private Set<String> userRoles;
	private Locale locale;
	private Date lastAccess;

	public UserSession(Certificate certificate) {
		this.sessionId = certificate.getSessionId();
		this.loginTime = certificate.getLoginTime();
		this.username = certificate.getUsername();
		this.firstname = certificate.getFirstname();
		this.lastname = certificate.getLastname();
		this.source = certificate.getSource();
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

	public String getSource() {
		return this.source;
	}

	public Set<String> getUserRoles() {
		return userRoles;
	}

	public JsonObject toJson() {
		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty("sessionId", this.sessionId);
		jsonObject.addProperty("loginTime", ISO8601FormatFactory.getInstance().formatDate(this.loginTime));
		jsonObject.addProperty("username", this.username);
		jsonObject.addProperty("firstname", this.firstname);
		jsonObject.addProperty("lastname", this.lastname);
		jsonObject.addProperty("source", this.source);

		jsonObject.addProperty("locale", this.locale.toString());
		jsonObject.addProperty("lastAccess", ISO8601FormatFactory.getInstance().formatDate(this.lastAccess));

		JsonArray rolesJ = new JsonArray();
		for (String role : this.userRoles) {
			rolesJ.add(role);
		}
		jsonObject.add("roles", rolesJ);

		return jsonObject;
	}
}
