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

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import li.strolch.privilege.model.Certificate;
import li.strolch.utils.iso8601.ISO8601;

import java.time.ZonedDateTime;
import java.util.Locale;
import java.util.Set;

public record UserSession(boolean keepAlive, String sessionId, ZonedDateTime loginTime, String username,
						  String firstName, String lastName, String source, Set<String> userRoles, Locale locale,
						  ZonedDateTime lastAccess) {

	public static UserSession valueOf(Certificate certificate) {
		return new UserSession(certificate.isKeepAlive(), certificate.getSessionId(), certificate.getLoginTime(),
				certificate.getUsername(), certificate.getFirstname(), certificate.getLastname(),
				certificate.getSource(), certificate.getUserRoles(), certificate.getLocale(),
				certificate.getLastAccess());
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
