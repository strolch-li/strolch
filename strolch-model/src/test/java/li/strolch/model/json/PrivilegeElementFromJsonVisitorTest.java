package li.strolch.model.json;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import li.strolch.privilege.model.CreatePersonalAccessTokenArgument;
import org.junit.Test;

import java.time.ZonedDateTime;
import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class PrivilegeElementFromJsonVisitorTest {

	@Test
	public void shouldParseCreatePersonalAccessTokenArgument() {
		JsonObject json = new JsonObject();
		json.addProperty("name", "test-token");
		json.addProperty("validFrom", "2026-06-01T12:00:00.000Z");
		json.addProperty("validTo", "2026-07-01T12:00:00.000Z");

		JsonArray roles = new JsonArray();
		roles.add("role1");
		json.add("roles", roles);

		JsonArray privileges = new JsonArray();
		privileges.add("priv1");
		json.add("privileges", privileges);

		PrivilegeElementFromJsonVisitor visitor = new PrivilegeElementFromJsonVisitor();
		CreatePersonalAccessTokenArgument arg = visitor.createPersonalAccessTokenArgumentFromJson(json);

		assertEquals("test-token", arg.name);
		assertTrue(arg.roles.contains("role1"));
		assertTrue(arg.privileges.contains("priv1"));
	}
}
