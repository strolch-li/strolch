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

package li.strolch.privilege.test;

import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.Restrictable;
import li.strolch.privilege.model.UserRep;
import li.strolch.privilege.test.model.TestRestrictable;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.*;

public class SsoHandlerTest extends AbstractPrivilegeTest {

	@BeforeClass
	public static void init() {
		removeConfigs(SsoHandlerTest.class.getSimpleName());
		prepareConfigs(SsoHandlerTest.class.getSimpleName(), "PrivilegeConfig.xml", "PrivilegeUsers.xml",
				"PrivilegeGroups.xml", "PrivilegeRoles.xml");
	}

	@AfterClass
	public static void destroy() {
		removeConfigs(SsoHandlerTest.class.getSimpleName());
	}

	@Before
	public void setup() {
		initialize(SsoHandlerTest.class.getSimpleName(), "PrivilegeConfig.xml");
	}

	@Test
	public void testSsoKnownUserAdmin() {

		try {
			Map<String, String> data = new HashMap<>();
			data.put("username", "admin");
			data.put("firstName", "Admin");
			data.put("lastName", "Istrator");
			data.put("groups", "AppUserLocationA");
			data.put("roles", "PrivilegeAdmin, AppUser");

			// auth
			Certificate cert = this.privilegeHandler.authenticateSingleSignOn(data, false);
			this.ctx = this.privilegeHandler.validate(cert);

			// validate action
			Restrictable restrictable = new TestRestrictable();
			this.ctx.validateAction(restrictable);

		} finally {
			// de-auth
			logout();
		}
	}

	@Test
	public void testSsoUnknownUserBob() {

		try {
			Map<String, String> data = new HashMap<>();
			data.put("username", "bob");
			data.put("firstName", "Bobby");
			data.put("lastName", "Someone");
			data.put("groups", "AppUserLocationA");
			data.put("roles", "PrivilegeAdmin, AppUser");

			// auth
			Certificate cert = this.privilegeHandler.authenticateSingleSignOn(data, false);
			this.ctx = this.privilegeHandler.validate(cert);

			// validate action
			Restrictable restrictable = new TestRestrictable();
			this.ctx.validateAction(restrictable);

		} finally {
			// de-auth
			logout();
		}
	}

	@Test
	public void testSsoUsernameChangeForExistingUserId() {
		String userId = "user-sso-123";
		try {
			Map<String, String> data = new HashMap<>();
			data.put("userId", userId);
			data.put("username", "bob");
			data.put("firstName", "Bobby");
			data.put("lastName", "Someone");
			data.put("groups", "AppUserLocationA");
			data.put("roles", "PrivilegeAdmin, AppUser");

			// auth initial user
			Certificate cert = this.privilegeHandler.authenticateSingleSignOn(data, false);
			assertEquals(userId, cert.getUserId());
			assertEquals("bob", cert.getUsername());

			this.ctx = this.privilegeHandler.validate(cert);
			UserRep bobUser = this.privilegeHandler.getUser(cert, "bob");
			assertNotNull(bobUser);
			assertEquals(userId, bobUser.getUserId());
			assertEquals("bob", bobUser.getUsername());
			assertNotNull(bobUser.getHistory().getFirstLogin());

			logout();

			// Now user changes name to "bob.smith" with same userId
			Map<String, String> updatedData = new HashMap<>();
			updatedData.put("userId", userId);
			updatedData.put("username", "bob.smith");
			updatedData.put("firstName", "Bobby");
			updatedData.put("lastName", "Smith");
			updatedData.put("groups", "AppUserLocationA");
			updatedData.put("roles", "PrivilegeAdmin, AppUser");

			Certificate updatedCert = this.privilegeHandler.authenticateSingleSignOn(updatedData, false);
			assertEquals(userId, updatedCert.getUserId());
			assertEquals("bob.smith", updatedCert.getUsername());

			this.ctx = this.privilegeHandler.validate(updatedCert);
			UserRep updatedUser = this.privilegeHandler.getUser(updatedCert, "bob.smith");
			assertNotNull(updatedUser);
			assertEquals(userId, updatedUser.getUserId());
			assertEquals("bob.smith", updatedUser.getUsername());
			assertEquals("Smith", updatedUser.getLastname());
			assertEquals(bobUser.getHistory().getFirstLogin(), updatedUser.getHistory().getFirstLogin());

			// Old username should no longer exist
			assertNull(this.privilegeHandler.getUser(updatedCert, "bob"));

			// Subsequent SSO with same updated username
			Certificate thirdCert = this.privilegeHandler.authenticateSingleSignOn(updatedData, false);
			assertEquals(userId, thirdCert.getUserId());
			assertEquals("bob.smith", thirdCert.getUsername());

		} finally {
			logout();
		}
	}
}
