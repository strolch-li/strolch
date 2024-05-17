package li.strolch.model.json;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import li.strolch.privilege.model.Privilege;
import li.strolch.privilege.model.RoleRep;
import li.strolch.privilege.model.UserRep;
import li.strolch.privilege.model.UserState;

import java.util.*;

public class PrivilegeElementFromJsonVisitor {

	public UserRep userRepFromJson(String string) {
		return userRepFromJson(JsonParser.parseString(string).getAsJsonObject());
	}

	public RoleRep roleRepFromJson(String string) {
		return roleRepFromJson(JsonParser.parseString(string).getAsJsonObject());
	}

	public Privilege privilegeFromJson(String string) {
		return privilegeFromJson(JsonParser.parseString(string).getAsJsonObject());
	}

	public RoleRep roleRepFromJson(JsonObject jsonObject) {

		JsonElement nameE = jsonObject.get("name");
		JsonElement privilegesE = jsonObject.get("privileges");

		String name = nameE == null ? null : nameE.getAsString().trim();

		Map<String, Privilege> privileges = new HashMap<>();
		if (privilegesE != null) {
			JsonArray privilegesArr = privilegesE.getAsJsonArray();
			for (JsonElement privilegeE : privilegesArr) {
				Privilege privilegeRep = privilegeFromJson(privilegeE.getAsJsonObject());
				privileges.put(privilegeRep.getName(), privilegeRep);
			}
		}

		return new RoleRep(name, privileges);
	}

	public Privilege privilegeFromJson(JsonObject privilegeJ) {

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

		return new Privilege(privilegeName, policy, allAllowed, denyList, allowList);
	}

	public UserRep userRepFromJson(JsonObject jsonObject) {

		JsonElement userIdE = jsonObject.get("userId");
		JsonElement userNameE = jsonObject.get("username");
		JsonElement firstNameE = jsonObject.get("firstname");
		JsonElement lastNameE = jsonObject.get("lastname");
		JsonElement userStateE = jsonObject.get("userState");
		JsonElement localeE = jsonObject.get("locale");
		JsonElement groupsE = jsonObject.get("groups");
		JsonElement rolesE = jsonObject.get("roles");
		JsonElement propertiesE = jsonObject.get("properties");

		String userId = userIdE == null ? null : userIdE.getAsString().trim();
		String username = userNameE == null ? null : userNameE.getAsString().trim();
		String firstname = firstNameE == null ? null : firstNameE.getAsString().trim();
		String lastname = lastNameE == null ? null : lastNameE.getAsString().trim();
		UserState userState = userStateE == null ? null : UserState.valueOf(userStateE.getAsString().trim());
		Locale locale = localeE == null ? null : Locale.forLanguageTag(localeE.getAsString().trim());

		Set<String> groups = jsonArrayToSet(groupsE);

		Set<String> roles = jsonArrayToSet(rolesE);

		Map<String, String> properties = null;
		if (propertiesE != null) {
			properties = new HashMap<>();
			JsonArray propertiesArr = propertiesE.getAsJsonArray();
			for (JsonElement propertyE : propertiesArr) {
				JsonObject property = propertyE.getAsJsonObject();
				properties.put(property.get("key").getAsString().trim(), property.get("value").getAsString().trim());
			}
		}

		return new UserRep(userId, username, firstname, lastname, userState, groups, roles, locale, properties, null);
	}

	private Set<String> jsonArrayToSet(JsonElement array) {
		if (array == null)
			return Set.of();

		Set<String> result = new HashSet<>();
		JsonArray rolesArr = array.getAsJsonArray();
		for (JsonElement role : rolesArr) {
			result.add(role.getAsString().trim());
		}

		return result;
	}
}
