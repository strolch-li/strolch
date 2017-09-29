package li.strolch.model.json;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import li.strolch.privilege.model.PrivilegeElementVisitor;
import li.strolch.privilege.model.PrivilegeRep;
import li.strolch.privilege.model.RoleRep;
import li.strolch.privilege.model.UserRep;

public class PrivilegeElementToJsonVisitor implements PrivilegeElementVisitor<JsonObject> {

	@Override
	public JsonObject visitUserRep(UserRep userRep) {
		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty("userId", userRep.getUserId());
		jsonObject.addProperty("username", userRep.getUsername());
		jsonObject.addProperty("firstname", userRep.getFirstname());
		jsonObject.addProperty("lastname", userRep.getLastname());
		jsonObject.addProperty("userState", userRep.getUserState().name());
		jsonObject.addProperty("locale", userRep.getLocale().toString());

		JsonArray rolesArr = new JsonArray();
		jsonObject.add("roles", rolesArr);
		for (String role : userRep.getRoles()) {
			rolesArr.add(new JsonPrimitive(role));
		}

		JsonArray propsArr = new JsonArray();
		jsonObject.add("properties", propsArr);
		for (String propKey : userRep.getPropertyKeySet()) {
			JsonObject propObj = new JsonObject();
			propObj.addProperty("key", propKey);
			propObj.addProperty("value", userRep.getProperty(propKey));
			propsArr.add(propObj);
		}

		return jsonObject;
	}

	@Override
	public JsonObject visitRoleRep(RoleRep roleRep) {
		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty("name", roleRep.getName());

		JsonArray privilegesJ = new JsonArray();
		roleRep.getPrivileges().forEach(p -> privilegesJ.add(p.accept(this)));
		jsonObject.add("privileges", privilegesJ);

		return jsonObject;
	}

	@Override
	public JsonObject visitPrivilegeRep(PrivilegeRep privilegeRep) {
		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty("name", privilegeRep.getName());
		jsonObject.addProperty("policy", privilegeRep.getName());
		jsonObject.addProperty("allAllowed", privilegeRep.getName());

		JsonArray denyListJ = new JsonArray();
		privilegeRep.getDenyList().forEach(denyListJ::add);
		jsonObject.add("denyList", denyListJ);

		JsonArray allowListJ = new JsonArray();
		privilegeRep.getAllowList().forEach(allowListJ::add);
		jsonObject.add("allowList", allowListJ);

		return jsonObject;
	}
}
