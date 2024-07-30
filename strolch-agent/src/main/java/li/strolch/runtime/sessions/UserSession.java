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
package li.strolch.runtime.sessions;

import java.time.ZonedDateTime;
import java.util.Locale;
import java.util.Set;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import li.strolch.privilege.model.Certificate;
import li.strolch.utils.iso8601.ISO8601;

public class UserSession {

	private final boolean keepAlive;
	private final String sessionId;
	private final ZonedDateTime loginTime;
	private final String username;
	private final String firstName;
	private final String lastName;
	private final String source;
	private final Set<String> userRoles;
	private final Locale locale;
	private final ZonedDateTime lastAccess;

	public UserSession(Certificate certificate) {
		this.sessionId = certificate.getSessionId();
		this.loginTime = certificate.getLoginTime();
		this.username = certificate.getUsername();
		this.firstName = certificate.getFirstname();
		this.lastName = certificate.getLastname();
		this.source = certificate.getSource();
		this.userRoles = certificate.getUserRoles();
		this.locale = certificate.getLocale();
		this.keepAlive = certificate.isKeepAlive();
		this.lastAccess = certificate.getLastAccess();
	}

	public Locale getLocale() {
		return locale;
	}

	public ZonedDateTime getLastAccess() {
		return lastAccess;
	}

	public String getSessionId() {
		return sessionId;
	}

	public ZonedDateTime getLoginTime() {
		return loginTime;
	}

	public String getUsername() {
		return username;
	}

	public String getFirstname() {
		return firstName;
	}

	public String getLastname() {
		return lastName;
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
		jsonObject.addProperty("loginTime", ISO8601.toString(this.loginTime));
		jsonObject.addProperty("username", this.username);
		jsonObject.addProperty("firstname", this.firstName);
		jsonObject.addProperty("lastname", this.lastName);
		jsonObject.addProperty("source", this.source);
		jsonObject.addProperty("keepAlive", this.keepAlive);

		jsonObject.addProperty("locale", this.locale.toString());
		jsonObject.addProperty("lastAccess", ISO8601.toString(this.lastAccess));

		JsonArray rolesJ = new JsonArray();
		for (String role : this.userRoles) {
			rolesJ.add(role);
		}
		jsonObject.add("roles", rolesJ);

		return jsonObject;
	}
}
