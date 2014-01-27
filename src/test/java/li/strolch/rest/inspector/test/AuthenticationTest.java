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
import li.strolch.rest.model.LoginResult;
import li.strolch.rest.model.LogoutResult;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.representation.Form;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AuthenticationTest extends AbstractRestfulTest {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	@Test
	public void shouldAuthenticate() {

		Form loginForm = new Form();
		loginForm.add("username", "jill");
		loginForm.add("password", "jill");

		// login
		ClientResponse loginResponse = doPostForm("/strolch/authentication/login", loginForm);
		LoginResult loginResult = loginResponse.getEntity(LoginResult.class);
		assertNotNull(loginResult);
		assertEquals("jill", loginResult.getUsername());
		assertEquals(64, loginResult.getSessionId().length());
		assertNull(loginResult.getMsg());

		// logout
		Form logoutForm = new Form();
		logoutForm.add("username", "jill");
		logoutForm.add("sessionId", loginResult.getSessionId());
		ClientResponse logoutResponse = doPostForm("/strolch/authentication/logout", logoutForm);
		LogoutResult logoutResult = logoutResponse.getEntity(LogoutResult.class);
		assertNotNull(logoutResult);
		assertNull(logoutResult.getMsg());
	}

	@Test
	public void shouldNotAuthenticate() {

		exception.expect(RuntimeException.class);
		exception.expectMessage("Authentication credentials are invalid");

		Form loginForm = new Form();
		loginForm.add("username", "admin");
		loginForm.add("password", "blalba");

		// login
		doPostForm("/strolch/authentication/login", loginForm);
	}

	@Test
	public void shouldFailLogoutIllegalSession() {

		exception.expect(RuntimeException.class);
		exception.expectMessage("Illegal request for username jill and sessionId blabla");

		Form loginForm = new Form();
		loginForm.add("username", "jill");
		loginForm.add("password", "jill");

		// login
		ClientResponse loginResponse = doPostForm("/strolch/authentication/login", loginForm);
		LoginResult loginResult = loginResponse.getEntity(LoginResult.class);
		assertNotNull(loginResult);
		assertEquals("jill", loginResult.getUsername());
		assertEquals(64, loginResult.getSessionId().length());
		assertNull(loginResult.getMsg());

		// logout
		Form logoutForm = new Form();
		logoutForm.add("username", "jill");
		logoutForm.add("sessionId", "blabla");
		doPostForm("/strolch/authentication/logout", logoutForm);
	}
}
