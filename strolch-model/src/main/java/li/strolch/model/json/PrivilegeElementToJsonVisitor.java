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

public class PrivilegeElementToJsonVisitor implements PrivilegeElementVisitor<JsonObject> {

	@Override
	public JsonObject visitUserRep(UserRep userRep) {
		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty("userId", userRep.getUserId());
		jsonObject.addProperty("username", userRep.getUsername());
		jsonObject.addProperty("firstname", userRep.getFirstname());
		jsonObject.addProperty("lastname", userRep.getLastname());
		jsonObject.addProperty("userState", userRep.getUserState().name());
		jsonObject.addProperty("locale", userRep.getLocale().toLanguageTag());

		addSet(jsonObject, userRep.getGroups(), "groups");
		addSet(jsonObject, userRep.getRoles(), "roles");
		addProperties(userRep.getProperties(), jsonObject);
		addHistory(userRep, jsonObject);

		return jsonObject;
	}

	@Override
	public JsonObject visitRoleRep(RoleRep roleRep) {
		JsonObject jsonObject = new JsonObject();
		jsonObject.addProperty("name", roleRep.getName());
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
		jsonObject.addProperty("name", group.name());

		addSet(jsonObject, group.roles(), "roles");
		addProperties(group.getProperties(), jsonObject);

		return jsonObject;
	}

	@Override
	public JsonObject visitPrivilegeRep(Privilege privilegeRep) {
		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty("name", privilegeRep.getName());
		jsonObject.addProperty("policy", privilegeRep.getPolicy());
		jsonObject.addProperty("allAllowed", privilegeRep.isAllAllowed());

		addList(jsonObject, privilegeRep.getDenyList(), "denyList");
		addList(jsonObject, privilegeRep.getAllowList(), "allowList");

		return jsonObject;
	}

	private void addPrivileges(Collection<Privilege> privileges, JsonObject jsonObject) {
		JsonArray privilegesJ = new JsonArray();
		privileges
				.stream()
				.sorted(comparing(p -> p.name().toLowerCase()))
				.forEach(p -> privilegesJ.add(p.accept(this)));
		jsonObject.add("privileges", privilegesJ);
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
		jsonObject.add("history", historyJ);
		UserHistory history = userRep.getHistory();
		historyJ.addProperty("firstLogin", ISO8601.toString(history.getFirstLogin()));
		historyJ.addProperty("lastLogin", ISO8601.toString(history.getLastLogin()));
		historyJ.addProperty("lastPasswordChange", ISO8601.toString(history.getLastPasswordChange()));
	}

	private static void addProperties(Map<String, String> properties, JsonObject jsonObject) {
		JsonArray propsArr = new JsonArray();
		jsonObject.add("properties", propsArr);
		properties.keySet().stream().sorted(String::compareToIgnoreCase).forEach(propKey -> {
			JsonObject propObj = new JsonObject();
			propObj.addProperty("key", propKey);
			propObj.addProperty("value", properties.get(propKey));
			propsArr.add(propObj);
		});
	}
}
