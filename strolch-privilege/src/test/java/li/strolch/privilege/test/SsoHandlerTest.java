/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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
import li.strolch.privilege.test.model.TestRestrictable;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

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
	public void testSsoAdmin() {

		try {
			Map<String, String> data = new HashMap<>();
			data.put("userId", "admin");
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
}
