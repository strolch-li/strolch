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

import jakarta.ws.rs.client.Entity;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.Status;
import java.util.Base64;

import com.google.gson.JsonObject;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class AuthenticationTest extends AbstractRestfulTest {

	@Test
	public void shouldAuthenticate() {
		String authToken = authenticate("jill", "jill");
		logout("jill", authToken);
	}

	@Test
	public void shouldNotAuthenticate() {

		// login
		JsonObject login = new JsonObject();
		login.addProperty("username", "jill");
		login.addProperty("password", Base64.getEncoder().encodeToString("blabla".getBytes()));
		Entity<String> entity = Entity.entity(login.toString(), MediaType.APPLICATION_JSON);

		Response result = target().path(AUTHENTICATION_PATH).request(MediaType.APPLICATION_JSON).post(entity);
		assertEquals(Status.UNAUTHORIZED.getStatusCode(), result.getStatus());
	}

	@Test
	public void shouldFailLogoutIllegalSession() {

		// logout
		Response result = target().path(AUTHENTICATION_PATH + "/blabla").request(MediaType.APPLICATION_JSON).delete();
		assertEquals(Status.UNAUTHORIZED.getStatusCode(), result.getStatus());
	}
}
