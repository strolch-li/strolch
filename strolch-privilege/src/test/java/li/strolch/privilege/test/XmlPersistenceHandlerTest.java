/*
 * Copyright (c) 2013-2026 Robert von Burg <eitch@eitchnet.ch>
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
import li.strolch.privilege.model.Group;
import li.strolch.privilege.model.Privilege;
import li.strolch.privilege.model.UserState;
import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserHistory;
import li.strolch.utils.helper.FileHelper;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.File;
import java.nio.file.Files;
import java.util.*;

import static li.strolch.privilege.helper.XmlConstants.*;
import static org.junit.Assert.*;

public class XmlPersistenceHandlerTest extends AbstractPrivilegeTest {

	private static final String TARGET_DST = XmlPersistenceHandlerTest.class.getSimpleName();
	private static final String BASE_PATH = "target/" + TARGET_DST;

	@BeforeClass
	public static void init() {
		removeConfigs(TARGET_DST);
		prepareConfigs(TARGET_DST, "PrivilegeConfig.xml", "PrivilegeUsers.xml", "PrivilegeGroups.xml",
				"PrivilegeRoles.xml");
	}

	@AfterClass
	public static void destroy() {
		removeConfigs(TARGET_DST);
	}

	@Before
	public void setup() {
		FileHelper.setAtomicMoveSupported(true);
		initialize(TARGET_DST, "PrivilegeConfig.xml");
	}

	@Test
	public void shouldPersistWhenAtomicMoveNotSupported() throws Exception {
		FileHelper.setAtomicMoveSupported(false);
		assertFalse(FileHelper.isAtomicMoveSupported());

		XmlPersistenceHandler handler = new XmlPersistenceHandler();
		Map<String, String> params = new HashMap<>();
		params.put(PARAM_BASE_PATH, BASE_PATH);
		params.put(PARAM_USERS_FILE, "PrivilegeUsers.xml");
		params.put(PARAM_GROUPS_FILE, "PrivilegeGroups.xml");
		params.put(PARAM_ROLES_FILE, "PrivilegeRoles.xml");
		handler.initialize(params);

		User newUser = new User("888", "fallbackuser", null, "Fallback", "User", UserState.ENABLED, Set.of("GroupA"),
				Set.of("AppUser"), Locale.getDefault(), Collections.emptyMap(), false, UserHistory.EMPTY);
		handler.addUser(newUser);

		boolean saved = handler.persist();
		assertTrue("Handler should have saved changes without atomic move", saved);

		File usersFile = new File(BASE_PATH, "PrivilegeUsers.xml");
		String updatedContent = Files.readString(usersFile.toPath());
		assertTrue("Updated file should contain fallback user", updatedContent.contains("fallbackuser"));
	}

	@Test
	public void shouldPersistChangesAtomically() throws Exception {
		XmlPersistenceHandler handler = new XmlPersistenceHandler();
		Map<String, String> params = new HashMap<>();
		params.put(PARAM_BASE_PATH, BASE_PATH);
		params.put(PARAM_USERS_FILE, "PrivilegeUsers.xml");
		params.put(PARAM_GROUPS_FILE, "PrivilegeGroups.xml");
		params.put(PARAM_ROLES_FILE, "PrivilegeRoles.xml");
		handler.initialize(params);

		File usersFile = new File(BASE_PATH, "PrivilegeUsers.xml");
		File usersTmpFile = new File(BASE_PATH, "PrivilegeUsers.xml.tmp");
		String originalContent = Files.readString(usersFile.toPath());
		assertFalse("Users file should not be empty", originalContent.isEmpty());
		assertFalse("Tmp file should not exist initially", usersTmpFile.exists());

		// Add a new user and persist
		User newUser = new User("999", "newuser", null, "New", "User", UserState.ENABLED, Set.of("GroupA"),
				Set.of("AppUser"), Locale.getDefault(), Collections.emptyMap(), false, UserHistory.EMPTY);
		handler.addUser(newUser);

		boolean saved = handler.persist();
		assertTrue("Handler should have saved changes", saved);
		assertFalse("Tmp file should be removed after atomic move", usersTmpFile.exists());

		String updatedContent = Files.readString(usersFile.toPath());
		assertTrue("Updated file should contain new user", updatedContent.contains("newuser"));

		// Reload to verify integrity
		XmlPersistenceHandler reloadedHandler = new XmlPersistenceHandler();
		reloadedHandler.initialize(params);
		assertNotNull("Reloaded handler should contain new user", reloadedHandler.getUser("newuser"));
	}

	@Test
	public void shouldPersistGroupsAtomically() throws Exception {
		XmlPersistenceHandler handler = new XmlPersistenceHandler();
		Map<String, String> params = new HashMap<>();
		params.put(PARAM_BASE_PATH, BASE_PATH);
		params.put(PARAM_USERS_FILE, "PrivilegeUsers.xml");
		params.put(PARAM_GROUPS_FILE, "PrivilegeGroups.xml");
		params.put(PARAM_ROLES_FILE, "PrivilegeRoles.xml");
		handler.initialize(params);

		File groupsFile = new File(BASE_PATH, "PrivilegeGroups.xml");
		File groupsTmpFile = new File(BASE_PATH, "PrivilegeGroups.xml.tmp");
		assertFalse("Tmp file should not exist initially", groupsTmpFile.exists());

		Group newGroup = new Group("GroupNew", Set.of("AppUser"), Collections.emptyMap());
		handler.addGroup(newGroup);

		boolean saved = handler.persist();
		assertTrue("Handler should have saved changes", saved);
		assertFalse("Tmp file should be removed after atomic move", groupsTmpFile.exists());

		String updatedContent = Files.readString(groupsFile.toPath());
		assertTrue("Updated file should contain new group", updatedContent.contains("GroupNew"));

		XmlPersistenceHandler reloadedHandler = new XmlPersistenceHandler();
		reloadedHandler.initialize(params);
		assertNotNull("Reloaded handler should contain new group", reloadedHandler.getGroup("GroupNew"));
	}

	@Test
	public void shouldPersistRolesAtomically() throws Exception {
		XmlPersistenceHandler handler = new XmlPersistenceHandler();
		Map<String, String> params = new HashMap<>();
		params.put(PARAM_BASE_PATH, BASE_PATH);
		params.put(PARAM_USERS_FILE, "PrivilegeUsers.xml");
		params.put(PARAM_GROUPS_FILE, "PrivilegeGroups.xml");
		params.put(PARAM_ROLES_FILE, "PrivilegeRoles.xml");
		handler.initialize(params);

		File rolesFile = new File(BASE_PATH, "PrivilegeRoles.xml");
		File rolesTmpFile = new File(BASE_PATH, "PrivilegeRoles.xml.tmp");
		assertFalse("Tmp file should not exist initially", rolesTmpFile.exists());

		Privilege priv = new Privilege("TestPriv", "DefaultPrivilege", true, Collections.emptySet(),
				Collections.emptySet());
		Role newRole = new Role("RoleNew", Map.of("TestPriv", priv));
		handler.addRole(newRole);

		boolean saved = handler.persist();
		assertTrue("Handler should have saved changes", saved);
		assertFalse("Tmp file should be removed after atomic move", rolesTmpFile.exists());

		String updatedContent = Files.readString(rolesFile.toPath());
		assertTrue("Updated file should contain new role", updatedContent.contains("RoleNew"));

		XmlPersistenceHandler reloadedHandler = new XmlPersistenceHandler();
		reloadedHandler.initialize(params);
		assertNotNull("Reloaded handler should contain new role", reloadedHandler.getRole("RoleNew"));
	}

	@Test
	public void shouldNotTruncateFileOnFailure() throws Exception {
		XmlPersistenceHandler handler = new XmlPersistenceHandler();
		Map<String, String> params = new HashMap<>();
		params.put(PARAM_BASE_PATH, BASE_PATH);
		params.put(PARAM_USERS_FILE, "PrivilegeUsers.xml");
		params.put(PARAM_GROUPS_FILE, "PrivilegeGroups.xml");
		params.put(PARAM_ROLES_FILE, "PrivilegeRoles.xml");
		handler.initialize(params);

		File usersFile = new File(BASE_PATH, "PrivilegeUsers.xml");
		String contentBefore = Files.readString(usersFile.toPath());
		long sizeBefore = usersFile.length();
		assertTrue("Initial file size must be > 0", sizeBefore > 0);

		// Verify existing file is untouched when no changes are dirty
		assertFalse(handler.persist());
		assertEquals("File size should remain unchanged", sizeBefore, usersFile.length());
		assertEquals("File content should remain identical", contentBefore, Files.readString(usersFile.toPath()));
	}
}
