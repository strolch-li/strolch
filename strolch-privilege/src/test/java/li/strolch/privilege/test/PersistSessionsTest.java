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

import li.strolch.privilege.handler.XmlPersistenceHandler;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.Usage;
import li.strolch.privilege.model.UserState;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.File;
import java.time.ZonedDateTime;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import static li.strolch.privilege.helper.XmlConstants.*;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class PersistSessionsTest extends AbstractPrivilegeTest {

	@BeforeClass
	public static void init() {
		removeConfigs(PersistSessionsTest.class.getSimpleName());
		prepareConfigs(PersistSessionsTest.class.getSimpleName(), "PrivilegeConfig.xml", "PrivilegeUsers.xml",
				"PrivilegeGroups.xml", "PrivilegeRoles.xml");
	}

	@AfterClass
	public static void destroy() {
		removeConfigs(PersistSessionsTest.class.getSimpleName());
	}

	@Before
	public void setup() {
		initialize(PersistSessionsTest.class.getSimpleName(), "PrivilegeConfig.xml");
	}

	@Test
	public void shouldPersistAndReloadSessions() throws InterruptedException {

		// assert no sessions file
		File sessionsFile = new File("target/PersistSessionsTest/PrivilegeSessions.xml");
		assertFalse("Sessions File should not yet exist", sessionsFile.exists());

		// login and assert sessions file was written
		login("admin", "admin".toCharArray());
		this.privilegeHandler.validate(ctx.getCertificate());
		// persisting is async, once per second
		Thread.sleep(1200L);
		assertTrue("Sessions File should have been created!", sessionsFile.isFile());

		// re-initialize and assert still logged in
		initialize(PersistSessionsTest.class.getSimpleName(), "PrivilegeConfig.xml");
		this.privilegeHandler.validate(ctx.getCertificate());
	}

	@Test
	public void shouldNotPersistSessionsWhenPersistSessionsFalse() throws Exception {
		String testTarget = "XmlSessionDisabledTest";
		prepareConfigs(testTarget, "PrivilegeConfig.xml", "PrivilegeUsers.xml", "PrivilegeGroups.xml",
				"PrivilegeRoles.xml");
		String testBasePath = "target/" + testTarget;

		XmlPersistenceHandler handler = new XmlPersistenceHandler();
		handler.initialize(Map.of(PARAM_BASE_PATH, testBasePath, PARAM_USERS_FILE, PARAM_USERS_FILE_DEF,
				PARAM_ROLES_FILE, PARAM_ROLES_FILE_DEF, PARAM_GROUPS_FILE, PARAM_GROUPS_FILE_DEF,
				PARAM_PERSIST_SESSIONS, "false"));

		Certificate cert = new Certificate(Usage.ANY, "disabled-xml-session-123", "admin", "admin", "First",
				"Last", UserState.ENABLED, "token-123", "127.0.0.1", ZonedDateTime.now(), false, Locale.ENGLISH,
				Set.of(), Set.of(), Map.of());

		handler.addSession(cert);
		boolean persisted = handler.persist();
		assertFalse("handler.persist() should return false when persistSessions is false", persisted);

		File sessionFile = new File(testBasePath, PARAM_SESSIONS_FILE_DEF);
		assertFalse("Sessions file should NOT exist when persistSessions is false", sessionFile.exists());
		assertTrue("getAllSessions should return empty list when persistSessions is false",
				handler.getAllSessions().isEmpty());

		removeConfigs(testTarget);
	}
}
