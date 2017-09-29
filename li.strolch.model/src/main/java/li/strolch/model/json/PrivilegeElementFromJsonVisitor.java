package li.strolch.model.json;

import java.util.*;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import li.strolch.privilege.model.UserRep;
import li.strolch.privilege.model.UserState;

public class PrivilegeElementFromJsonVisitor {

	public UserRep userRepFromJson(String string) {
		return userRepFromJson(new JsonParser().parse(string).getAsJsonObject());
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

		String userId = userIdE == null ? null : userIdE.getAsString();
		String username = userNameE == null ? null : userNameE.getAsString();
		String firstname = firstNameE == null ? null : firstNameE.getAsString();
		String lastname = lastNameE == null ? null : lastNameE.getAsString();
		UserState userState = userStateE == null ? null : UserState.valueOf(userStateE.getAsString());
		Locale locale = localeE == null ? null : new Locale(localeE.getAsString());

		Set<String> roles = null;
		if (rolesE != null) {
			roles = new HashSet<>();
			JsonArray rolesArr = rolesE.getAsJsonArray();
			for (JsonElement role : rolesArr) {
				roles.add(role.getAsString());
			}
		}

		Map<String, String> properties = null;
		if (propertiesE != null) {
			properties = new HashMap<>();
			JsonArray propertiesArr = propertiesE.getAsJsonArray();
			for (JsonElement propertyE : propertiesArr) {
				JsonObject property = propertyE.getAsJsonObject();
				properties.put(property.get("key").getAsString(), property.get("value").getAsString());
			}
		}

		return new UserRep(userId, username, firstname, lastname, userState, roles, locale, properties);
	}
}
