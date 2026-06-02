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
package li.strolch.rest.endpoint;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import jakarta.ws.rs.client.Entity;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import li.strolch.rest.inspector.test.AbstractRestfulTest;
import li.strolch.utils.iso8601.ISO8601;
import org.junit.Test;

import java.time.ZonedDateTime;

import static org.junit.Assert.*;

public class PersonalAccessTokenResourceTest extends AbstractRestfulTest {

	@Override
	protected void logout(String authToken) {
		logout("jill", authToken);
	}

	@Test
	public void shouldManagePersonalAccessTokens() {
		String authToken = authenticate();

		try {
			// 1. Get tokens (should be empty)
			try (Response response = target()
					.path("strolch/privilege/personal-access-tokens")
					.request(MediaType.APPLICATION_JSON)
					.header("Authorization", authToken)
					.get()) {
				assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
				JsonArray tokens = JsonParser.parseString(response.readEntity(String.class)).getAsJsonArray();
				assertEquals(0, tokens.size());
			}

			// 2. Create a token
			JsonObject createArg = new JsonObject();
			createArg.addProperty("name", "REST Test Token");
			createArg.addProperty("validFrom", ISO8601.toString(ZonedDateTime.now()));
			createArg.addProperty("validTo", ISO8601.toString(ZonedDateTime.now().plusDays(7)));

			try (Response response = target()
					.path("strolch/privilege/personal-access-tokens")
					.request(MediaType.APPLICATION_JSON)
					.header("Authorization", authToken)
					.post(Entity.json(createArg.toString()))) {
				if (response.getStatus() != Response.Status.CREATED.getStatusCode()) {
					System.out.println("[DEBUG_LOG] Create PAT failed: " + response.getStatus());
					System.out.println("[DEBUG_LOG] Response: " + response.readEntity(String.class));
				}
				assertEquals(Response.Status.CREATED.getStatusCode(), response.getStatus());
				JsonObject result = JsonParser.parseString(response.readEntity(String.class)).getAsJsonObject();
				String rawToken = result.get("token").getAsString();
				assertNotNull(rawToken);
				assertTrue(rawToken.contains(":"));
			}

			// 3. Get tokens (should have 1)
			String tokenId;
			try (Response response = target()
					.path("strolch/privilege/personal-access-tokens")
					.request(MediaType.APPLICATION_JSON)
					.header("Authorization", authToken)
					.get()) {
				assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
				JsonArray tokens = JsonParser.parseString(response.readEntity(String.class)).getAsJsonArray();
				assertEquals(1, tokens.size());
				JsonObject token = tokens.get(0).getAsJsonObject();
				assertEquals("REST Test Token", token.get("name").getAsString());
				tokenId = token.get("tokenId").getAsString();
			}

			// 4. Remove the token
			try (Response response = target()
					.path("strolch/privilege/personal-access-tokens/" + tokenId)
					.request(MediaType.APPLICATION_JSON)
					.header("Authorization", authToken)
					.delete()) {
				assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
			}

			// 5. Get tokens (should be empty again)
			try (Response response = target()
					.path("strolch/privilege/personal-access-tokens")
					.request(MediaType.APPLICATION_JSON)
					.header("Authorization", authToken)
					.get()) {
				assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
				JsonArray tokens = JsonParser.parseString(response.readEntity(String.class)).getAsJsonArray();
				assertEquals(0, tokens.size());
			}

		} finally {
			logout(authToken);
		}
	}
}
