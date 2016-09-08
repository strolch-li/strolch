/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.rest.inspector.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.Base64;

import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.junit.Test;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class AuthenticationTest extends AbstractRestfulTest {

	private static final String ROOT_PATH = "strolch/authentication";

	@Test
	public void shouldAuthenticate() {

		// login
		JsonObject login = new JsonObject();
		login.addProperty("username", "jill");
		login.addProperty("password", Base64.getEncoder().encodeToString("jill".getBytes()));
		Entity<String> entity = Entity.entity(login.toString(), MediaType.APPLICATION_JSON);

		Response result = target().path(ROOT_PATH).request(MediaType.APPLICATION_JSON).post(entity);
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		JsonObject loginResult = new JsonParser().parse(result.readEntity(String.class)).getAsJsonObject();
		assertNotNull(loginResult);
		assertEquals("jill", loginResult.get("username").getAsString());
		assertEquals(64, loginResult.get("authToken").getAsString().length());
		assertNull(loginResult.get("msg"));

		// logout
		result = target().path(ROOT_PATH + "/" + loginResult.get("authToken").getAsString())
				.request(MediaType.APPLICATION_JSON).delete();
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		assertNotNull(loginResult);
		JsonObject logoutResult = new JsonParser().parse(result.readEntity(String.class)).getAsJsonObject();
		assertNotNull(logoutResult);
		assertEquals("jill has been logged out.", logoutResult.get("msg").getAsString());
	}

	@Test
	public void shouldNotAuthenticate() {

		// login
		JsonObject login = new JsonObject();
		login.addProperty("username", "jill");
		login.addProperty("password", Base64.getEncoder().encodeToString("blabla".getBytes()));
		Entity<String> entity = Entity.entity(login.toString(), MediaType.APPLICATION_JSON);

		Response result = target().path(ROOT_PATH).request(MediaType.APPLICATION_JSON).post(entity);
		assertEquals(Status.UNAUTHORIZED.getStatusCode(), result.getStatus());
	}

	@Test
	public void shouldFailLogoutIllegalSession() {

		// login
		JsonObject login = new JsonObject();
		login.addProperty("username", "jill");
		login.addProperty("password", Base64.getEncoder().encodeToString("jill".getBytes()));
		Entity<String> entity = Entity.entity(login.toString(), MediaType.APPLICATION_JSON);

		Response result = target().path(ROOT_PATH).request(MediaType.APPLICATION_JSON).post(entity);
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		JsonObject loginResult = new JsonParser().parse(result.readEntity(String.class)).getAsJsonObject();
		assertNotNull(loginResult);
		assertEquals("jill", loginResult.get("username").getAsString());
		assertEquals(64, loginResult.get("authToken").getAsString().length());
		assertNull(loginResult.get("msg"));

		// logout
		result = target().path(ROOT_PATH + "/blabla").request(MediaType.APPLICATION_JSON).delete();
		assertEquals(Status.UNAUTHORIZED.getStatusCode(), result.getStatus());
	}
}
