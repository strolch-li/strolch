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

import javax.ws.rs.client.Entity;
import javax.ws.rs.core.Form;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import li.strolch.rest.model.LoginResult;
import li.strolch.rest.model.LogoutResult;

import org.junit.Test;

//import com.sun.jersey.api.client.ClientResponse;
//import com.sun.jersey.api.representation.Form;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AuthenticationTest extends AbstractRestfulTest {

	private static final String ROOT_PATH = "strolch/authentication";

	@Test
	public void shouldAuthenticate() {

		// login
		Form loginForm = new Form();
		loginForm.param("username", "jill");
		loginForm.param("password", "jill");
		Entity<Form> entity = Entity.entity(loginForm, MediaType.APPLICATION_FORM_URLENCODED);
		Response result = target().path(ROOT_PATH + "/login").request(MediaType.APPLICATION_JSON).post(entity);
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		LoginResult loginResult = result.readEntity(LoginResult.class);
		assertNotNull(loginResult);
		assertEquals("jill", loginResult.getUsername());
		assertEquals(64, loginResult.getSessionId().length());
		assertNull(loginResult.getMsg());

		// logout
		Form logoutForm = new Form();
		logoutForm.param("username", "jill");
		logoutForm.param("sessionId", loginResult.getSessionId());
		entity = Entity.entity(logoutForm, MediaType.APPLICATION_FORM_URLENCODED);
		result = target().path(ROOT_PATH + "/logout").request(MediaType.APPLICATION_JSON).post(entity);
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		assertNotNull(loginResult);
		LogoutResult logoutResult = result.readEntity(LogoutResult.class);
		assertNotNull(logoutResult);
		assertNull(logoutResult.getMsg());
	}

	@Test
	public void shouldNotAuthenticate() {

		// login
		Form loginForm = new Form();
		loginForm.param("username", "admin");
		loginForm.param("password", "blalba");
		Entity<Form> entity = Entity.entity(loginForm, MediaType.APPLICATION_FORM_URLENCODED);
		Response result = target().path(ROOT_PATH + "/login").request(MediaType.APPLICATION_JSON).post(entity);
		assertEquals(Status.UNAUTHORIZED.getStatusCode(), result.getStatus());
		LogoutResult logoutResult = result.readEntity(LogoutResult.class);
		assertNotNull(logoutResult);
		assertEquals("Could not log in due to: Authentication credentials are invalid", logoutResult.getMsg());
	}

	@Test
	public void shouldFailLogoutIllegalSession() {

		// login
		Form loginForm = new Form();
		loginForm.param("username", "jill");
		loginForm.param("password", "jill");
		Entity<Form> entity = Entity.entity(loginForm, MediaType.APPLICATION_FORM_URLENCODED);
		Response result = target().path(ROOT_PATH + "/login").request(MediaType.APPLICATION_JSON).post(entity);
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		LoginResult loginResult = result.readEntity(LoginResult.class);
		assertNotNull(loginResult);
		assertEquals("jill", loginResult.getUsername());
		assertEquals(64, loginResult.getSessionId().length());
		assertNull(loginResult.getMsg());

		// logout
		Form logoutForm = new Form();
		logoutForm.param("username", "jill");
		logoutForm.param("sessionId", "blabla");
		entity = Entity.entity(logoutForm, MediaType.APPLICATION_FORM_URLENCODED);
		result = target().path(ROOT_PATH + "/logout").request(MediaType.APPLICATION_JSON).post(entity);
		assertEquals(Status.UNAUTHORIZED.getStatusCode(), result.getStatus());
		LogoutResult logoutResult = result.readEntity(LogoutResult.class);
		assertNotNull(logoutResult);
		assertEquals("Could not logout due to: Illegal request for username jill and sessionId blabla",
				logoutResult.getMsg());
	}
}
