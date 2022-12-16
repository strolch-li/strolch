package li.strolch.model.json;

import java.util.*;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import li.strolch.privilege.model.PrivilegeRep;
import li.strolch.privilege.model.RoleRep;
import li.strolch.privilege.model.UserRep;
import li.strolch.privilege.model.UserState;

public class PrivilegeElementFromJsonVisitor {

	public UserRep userRepFromJson(String string) {
		return userRepFromJson(JsonParser.parseString(string).getAsJsonObject());
	}

	public RoleRep roleRepFromJson(String string) {
		return roleRepFromJson(JsonParser.parseString(string).getAsJsonObject());
	}

	public PrivilegeRep privilegeRepFromJson(String string) {
		return privilegeRepFromJson(JsonParser.parseString(string).getAsJsonObject());
	}

	public RoleRep roleRepFromJson(JsonObject jsonObject) {

		JsonElement nameE = jsonObject.get("name");
		JsonElement privilegesE = jsonObject.get("privileges");

		String name = nameE == null ? null : nameE.getAsString().trim();

		List<PrivilegeRep> privileges = new ArrayList<>();
		if (privilegesE != null) {
			JsonArray privilegesArr = privilegesE.getAsJsonArray();
			for (JsonElement privilegeE : privilegesArr) {
				privileges.add(privilegeRepFromJson(privilegeE.getAsJsonObject()));
			}
		}

		return new RoleRep(name, privileges);
	}

	public PrivilegeRep privilegeRepFromJson(JsonObject privilegeJ) {

		JsonElement privilegeNameE = privilegeJ.get("name");
		JsonElement policyE = privilegeJ.get("policy");
		JsonElement allAllowedE = privilegeJ.get("allAllowed");
		JsonElement denyListE = privilegeJ.get("denyList");
		JsonElement allowListE = privilegeJ.get("allowList");

		String privilegeName = privilegeNameE == null ? null : privilegeNameE.getAsString().trim();
		String policy = policyE == null ? null : policyE.getAsString().trim();
		boolean allAllowed = allAllowedE != null && allAllowedE.getAsBoolean();

		Set<String> denyList = new HashSet<>();
		if (denyListE != null) {
			JsonArray denyListArr = denyListE.getAsJsonArray();
			for (JsonElement denyValueE : denyListArr) {
				denyList.add(denyValueE.getAsString().trim());
			}
		}

		Set<String> allowList = new HashSet<>();
		if (allowListE != null) {
			JsonArray allowListArr = allowListE.getAsJsonArray();
			for (JsonElement allowValueE : allowListArr) {
				allowList.add(allowValueE.getAsString().trim());
			}
		}

		return new PrivilegeRep(privilegeName, policy, allAllowed, denyList, allowList);
	}

	public UserRep userRepFromJson(JsonObject jsonObject) {

		JsonElement userIdE = jsonObject.get("userId");
		JsonElement userNameE = jsonObject.get("username");
		JsonElement firstNameE = jsonObject.get("firstname");
		JsonElement lastNameE = jsonObject.get("lastname");
		JsonElement userStateE = jsonObject.get("userState");
		JsonElement localeE = jsonObject.get("locale");
		JsonElement rolesE = jsonObject.get("roles");
		JsonElement propertiesE = jsonObject.get("properties");

		String userId = userIdE == null ? null : userIdE.getAsString().trim();
		String username = userNameE == null ? null : userNameE.getAsString().trim();
		String firstname = firstNameE == null ? null : firstNameE.getAsString().trim();
		String lastname = lastNameE == null ? null : lastNameE.getAsString().trim();
		UserState userState = userStateE == null ? null : UserState.valueOf(userStateE.getAsString().trim());
		Locale locale = localeE == null ? null : new Locale(localeE.getAsString().trim());

		Set<String> roles = null;
		if (rolesE != null) {
			roles = new HashSet<>();
			JsonArray rolesArr = rolesE.getAsJsonArray();
			for (JsonElement role : rolesArr) {
				roles.add(role.getAsString().trim());
			}
		}

		Map<String, String> properties = null;
		if (propertiesE != null) {
			properties = new HashMap<>();
			JsonArray propertiesArr = propertiesE.getAsJsonArray();
			for (JsonElement propertyE : propertiesArr) {
				JsonObject property = propertyE.getAsJsonObject();
				properties.put(property.get("key").getAsString().trim(), property.get("value").getAsString().trim());
			}
		}

		return new UserRep(userId, username, firstname, lastname, userState, roles, locale, properties, null);
	}
}
