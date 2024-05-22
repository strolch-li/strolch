package li.strolch.model.json;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import li.strolch.privilege.model.*;
import li.strolch.privilege.model.internal.UserHistory;
import li.strolch.utils.iso8601.ISO8601;

import java.util.Map;

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

		JsonArray rolesArr = new JsonArray();
		jsonObject.add("roles", rolesArr);
		userRep.getRoles().stream().sorted(String::compareToIgnoreCase).map(JsonPrimitive::new).forEach(rolesArr::add);

		addProperties(userRep.getProperties(), jsonObject);

		JsonObject historyJ = new JsonObject();
		jsonObject.add("history", historyJ);
		UserHistory history = userRep.getHistory();
		historyJ.addProperty("firstLogin", ISO8601.toString(history.getFirstLogin()));
		historyJ.addProperty("lastLogin", ISO8601.toString(history.getLastLogin()));
		historyJ.addProperty("lastPasswordChange", ISO8601.toString(history.getLastPasswordChange()));

		return jsonObject;
	}

	@Override
	public JsonObject visitRoleRep(RoleRep roleRep) {
		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty("name", roleRep.getName());

		JsonArray privilegesJ = new JsonArray();
		roleRep.getPrivileges().values().forEach(p -> privilegesJ.add(p.accept(this)));
		jsonObject.add("privileges", privilegesJ);

		return jsonObject;
	}

	@Override
	public JsonObject visitGroup(Group group) {
		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty("name", group.name());

		JsonArray rolesJ = new JsonArray();
		group.roles().forEach(rolesJ::add);
		jsonObject.add("roles", rolesJ);

		addProperties(group.getProperties(), jsonObject);

		return jsonObject;
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

	@Override
	public JsonObject visitPrivilegeRep(Privilege privilegeRep) {
		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty("name", privilegeRep.getName());
		jsonObject.addProperty("policy", privilegeRep.getPolicy());
		jsonObject.addProperty("allAllowed", privilegeRep.isAllAllowed());

		JsonArray denyListJ = new JsonArray();
		privilegeRep.getDenyList().forEach(denyListJ::add);
		jsonObject.add("denyList", denyListJ);

		JsonArray allowListJ = new JsonArray();
		privilegeRep.getAllowList().forEach(allowListJ::add);
		jsonObject.add("allowList", allowListJ);

		return jsonObject;
	}

	@Override
	public JsonObject visitUserPrivileges(UserPrivileges userPrivileges) {
		JsonObject jsonObject = userPrivileges.userRep().accept(this);

		JsonArray privilegesJ = new JsonArray();
		userPrivileges.privileges().forEach(privilege -> privilegesJ.add(privilege.accept(this)));
		jsonObject.add("privileges", privilegesJ);

		return jsonObject;
	}
}
