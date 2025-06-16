/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.model.json;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import li.strolch.privilege.model.*;
import li.strolch.privilege.model.internal.UserHistory;
import li.strolch.utils.iso8601.ISO8601;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import static java.util.Comparator.comparing;
import static li.strolch.model.Tags.Json.*;

public class PrivilegeElementToJsonVisitor implements PrivilegeElementVisitor<JsonObject> {

	@Override
	public JsonObject visitUserRep(UserRep userRep) {
		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty(USER_ID, userRep.getUserId());
		jsonObject.addProperty(USERNAME, userRep.getUsername());
		jsonObject.addProperty(FIRSTNAME, userRep.getFirstname());
		jsonObject.addProperty(LASTNAME, userRep.getLastname());
		jsonObject.addProperty(USER_STATE, userRep.getUserState().name());
		jsonObject.addProperty(LOCALE, userRep.getLocale().toLanguageTag());

		addSet(jsonObject, userRep.getGroups(), GROUPS);
		addSet(jsonObject, userRep.getRoles(), ROLES);
		addProperties(userRep.getProperties(), jsonObject);
		addHistory(userRep, jsonObject);

		return jsonObject;
	}

	@Override
	public JsonObject visitRoleRep(RoleRep roleRep) {
		JsonObject jsonObject = new JsonObject();
		jsonObject.addProperty(NAME, roleRep.getName());
		addPrivileges(roleRep.getPrivileges().values(), jsonObject);
		return jsonObject;
	}

	@Override
	public JsonObject visitUserPrivileges(UserPrivileges userPrivileges) {
		JsonObject jsonObject = userPrivileges.userRep().accept(this);
		addPrivileges(userPrivileges.privileges(), jsonObject);
		return jsonObject;
	}

	@Override
	public JsonObject visitGroupPrivileges(GroupPrivileges groupPrivileges) {
		JsonObject jsonObject = groupPrivileges.group().accept(this);
		addPrivileges(groupPrivileges.privileges(), jsonObject);
		return jsonObject;
	}

	@Override
	public JsonObject visitGroup(Group group) {
		JsonObject jsonObject = new JsonObject();
		jsonObject.addProperty(NAME, group.name());

		addSet(jsonObject, group.roles(), ROLES);
		addProperties(group.getProperties(), jsonObject);

		return jsonObject;
	}

	@Override
	public JsonObject visitPrivilegeRep(Privilege privilegeRep) {
		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty(NAME, privilegeRep.getName());
		jsonObject.addProperty(POLICY, privilegeRep.getPolicy());
		jsonObject.addProperty(ALL_ALLOWED, privilegeRep.isAllAllowed());

		addList(jsonObject, privilegeRep.getDenyList(), DENY_LIST);
		addList(jsonObject, privilegeRep.getAllowList(), ALLOW_LIST);

		return jsonObject;
	}

	private void addPrivileges(Collection<Privilege> privileges, JsonObject jsonObject) {
		JsonArray privilegesJ = new JsonArray();
		privileges
				.stream()
				.sorted(comparing(p -> p.name().toLowerCase()))
				.forEach(p -> privilegesJ.add(p.accept(this)));
		jsonObject.add(PRIVILEGES, privilegesJ);
	}

	private static void addList(JsonObject jsonObject, Set<String> privilegeRep, String listName) {
		JsonArray listJ = new JsonArray();
		privilegeRep.stream().sorted(String::compareToIgnoreCase).forEach(listJ::add);
		jsonObject.add(listName, listJ);
	}

	private static void addSet(JsonObject jsonObject, Set<String> values, String name) {
		JsonArray listJ = new JsonArray();
		jsonObject.add(name, listJ);
		values.stream().sorted(String::compareToIgnoreCase).map(JsonPrimitive::new).forEach(listJ::add);
	}

	private static void addHistory(UserRep userRep, JsonObject jsonObject) {
		JsonObject historyJ = new JsonObject();
		jsonObject.add(HISTORY, historyJ);
		UserHistory history = userRep.getHistory();
		historyJ.addProperty(FIRST_LOGIN, ISO8601.toString(history.getFirstLogin()));
		historyJ.addProperty(LAST_LOGIN, ISO8601.toString(history.getLastLogin()));
		historyJ.addProperty(LAST_PASSWORD_CHANGE, ISO8601.toString(history.getLastPasswordChange()));
	}

	private static void addProperties(Map<String, String> properties, JsonObject jsonObject) {
		JsonArray propsArr = new JsonArray();
		jsonObject.add(PROPERTIES, propsArr);
		properties.keySet().stream().sorted(String::compareToIgnoreCase).forEach(propKey -> {
			JsonObject propObj = new JsonObject();
			propObj.addProperty(KEY, propKey);
			propObj.addProperty(VALUE, properties.get(propKey));
			propsArr.add(propObj);
		});
	}
}
