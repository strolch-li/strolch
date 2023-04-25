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
package li.strolch.privilege.test;

import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.junit.Assert.*;

import java.io.File;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.*;

import li.strolch.privilege.handler.DefaultEncryptionHandler;
import li.strolch.privilege.handler.MailUserChallengeHandler;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.handler.XmlPersistenceHandler;
import li.strolch.privilege.model.IPrivilege;
import li.strolch.privilege.model.UserState;
import li.strolch.privilege.model.internal.*;
import li.strolch.privilege.test.model.DummySsoHandler;
import li.strolch.privilege.xml.*;
import li.strolch.utils.helper.FileHelper;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.helper.XmlHelper;
import org.hamcrest.MatcherAssert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class XmlTest {

	public static final String TARGET_TEST = "target/test/";
	public static final String SRC_TEST = "src/test/resources/config/";
	public static final Logger logger = LoggerFactory.getLogger(XmlTest.class);

	@BeforeClass
	public static void init() {

		cleanUp();

		File tmpDir = new File("target/test");
		if (tmpDir.exists())
			FileHelper.deleteFile(tmpDir, false);
		if (!tmpDir.mkdirs())
			throw new IllegalStateException("Could not create temp dir " + tmpDir.getAbsolutePath());
	}

	public static void cleanUp() {

		File tmpDir = new File("target/test");
		if (!tmpDir.exists())
			return;

		File tmpFile = new File(TARGET_TEST + "PrivilegeTest.xml");
		if (tmpFile.exists() && !tmpFile.delete()) {
			throw new RuntimeException("Tmp still exists and can not be deleted at " + tmpFile.getAbsolutePath());
		}

		tmpFile = new File(TARGET_TEST + "PrivilegeUsersTest.xml");
		if (tmpFile.exists() && !tmpFile.delete()) {
			throw new RuntimeException("Tmp still exists and can not be deleted at " + tmpFile.getAbsolutePath());
		}

		tmpFile = new File(TARGET_TEST + "PrivilegeRolesTest.xml");
		if (tmpFile.exists() && !tmpFile.delete()) {
			throw new RuntimeException("Tmp still exists and can not be deleted at " + tmpFile.getAbsolutePath());
		}

		// and temporary parent
		if (!tmpDir.delete()) {
			throw new RuntimeException("Could not remove temporary parent for tmp " + tmpFile);
		}
	}

	@Test
	public void canReadConfig() {

		PrivilegeContainerModel containerModel = new PrivilegeContainerModel();
		PrivilegeConfigSaxReader saxReader = new PrivilegeConfigSaxReader(containerModel);
		File xmlFile = new File(SRC_TEST + "PrivilegeConfig.xml");
		XmlHelper.parseDocument(xmlFile, saxReader);
		logger.info(containerModel.toString());

		// assert all objects read
		assertNotNull(containerModel.getParameterMap());
		assertNotNull(containerModel.getPolicies());
		assertNotNull(containerModel.getEncryptionHandlerClassName());
		assertNotNull(containerModel.getEncryptionHandlerParameterMap());
		assertNotNull(containerModel.getPersistenceHandlerClassName());
		assertNotNull(containerModel.getPersistenceHandlerParameterMap());

		assertEquals(6, containerModel.getParameterMap().size());
		assertEquals(4, containerModel.getPolicies().size());
		assertEquals(3, containerModel.getEncryptionHandlerParameterMap().size());
		assertEquals(3, containerModel.getPersistenceHandlerParameterMap().size());
	}

	@Test
	public void canWriteConfig() {

		Map<String, String> parameterMap = new HashMap<>();
		Map<String, String> encryptionHandlerParameterMap = new HashMap<>();
		Map<String, String> persistenceHandlerParameterMap = new HashMap<>();

		parameterMap.put("autoPersistOnPasswordChange", "true");
		encryptionHandlerParameterMap.put("hashAlgorithm", "SHA-256");
		persistenceHandlerParameterMap.put("basePath", TARGET_TEST);
		persistenceHandlerParameterMap.put("modelXmlFile", "PrivilegeModel.xml");

		PrivilegeContainerModel containerModel = new PrivilegeContainerModel();
		containerModel.setParameterMap(parameterMap);
		containerModel.setEncryptionHandlerClassName(DefaultEncryptionHandler.class.getName());
		containerModel.setEncryptionHandlerParameterMap(encryptionHandlerParameterMap);
		containerModel.setPersistenceHandlerClassName(XmlPersistenceHandler.class.getName());
		containerModel.setPersistenceHandlerParameterMap(persistenceHandlerParameterMap);
		containerModel.setUserChallengeHandlerClassName(MailUserChallengeHandler.class.getName());
		containerModel.setSsoHandlerClassName(DummySsoHandler.class.getName());

		containerModel.addPolicy("DefaultPrivilege", "li.strolch.privilege.policy.DefaultPrivilege");

		File configFile = new File(TARGET_TEST + "PrivilegeTest.xml");
		PrivilegeConfigDomWriter configSaxWriter = new PrivilegeConfigDomWriter(containerModel, configFile);
		configSaxWriter.write();

		String fileHash = StringHelper.toHexString(FileHelper.hashFileSha256(configFile));
		assertEquals("dcb6b3ed7198e0a7c88bf5c61c0bd6f0d684415f2a2f29429879edc6bc795f06", fileHash);
	}

	@Test
	public void canReadUsers() {

		PrivilegeUsersSaxReader xmlHandler = new PrivilegeUsersSaxReader(true);
		File xmlFile = new File(SRC_TEST + "PrivilegeUsers.xml");
		XmlHelper.parseDocument(xmlFile, xmlHandler);

		Map<String, User> users = xmlHandler.getUsers();
		assertNotNull(users);

		assertEquals(4, users.size());

		//
		// users
		//

		// admin
		User admin = findUser("admin", users);
		assertEquals("1", admin.getUserId());
		assertEquals("admin", admin.getUsername());
		assertEquals("cb69962946617da006a2f95776d78b49e5ec7941d2bdb2d25cdb05f957f64344",
				StringHelper.toHexString(admin.getPassword()));
		assertEquals("61646d696e", StringHelper.toHexString(admin.getSalt()));
		assertEquals("Application", admin.getFirstname());
		assertEquals("Administrator", admin.getLastname());
		assertEquals(UserState.ENABLED, admin.getUserState());
		assertEquals("en-GB", admin.getLocale().toLanguageTag());
		MatcherAssert.assertThat(admin.getRoles(), containsInAnyOrder("PrivilegeAdmin", "AppUser"));
		Map<String, String> properties = admin.getProperties();
		assertEquals(new HashSet<>(Arrays.asList("organization", "organizationalUnit")), properties.keySet());
		assertEquals("eitchnet.ch", properties.get("organization"));
		assertEquals("Development", properties.get("organizationalUnit"));

		// system_admin
		User systemAdmin = findUser("system_admin", users);
		assertEquals("2", systemAdmin.getUserId());
		assertEquals("system_admin", systemAdmin.getUsername());
		assertNull(systemAdmin.getPassword());
		assertNull(systemAdmin.getSalt());
		assertEquals("System User", systemAdmin.getFirstname());
		assertEquals("Administrator", systemAdmin.getLastname());
		assertEquals(UserState.SYSTEM, systemAdmin.getUserState());
		assertEquals("en-GB", systemAdmin.getLocale().toLanguageTag());
		MatcherAssert.assertThat(systemAdmin.getRoles(), containsInAnyOrder("system_admin_privileges"));
		assertTrue(systemAdmin.getProperties().isEmpty());
	}

	@Test
	public void canReadRoles() {

		PrivilegeRolesSaxReader xmlHandler = new PrivilegeRolesSaxReader();
		File xmlFile = new File(SRC_TEST + "PrivilegeRoles.xml");
		XmlHelper.parseDocument(xmlFile, xmlHandler);

		Map<String, Role> roles = xmlHandler.getRoles();
		assertNotNull(roles);

		assertEquals(6, roles.size());

		// assert model

		//
		// roles
		//

		// PrivilegeAdmin
		Role privilegeAdmin = findRole("PrivilegeAdmin", roles);
		assertEquals("PrivilegeAdmin", privilegeAdmin.getName());
		assertEquals(18, privilegeAdmin.getPrivilegeNames().size());
		IPrivilege privilegeAction = privilegeAdmin.getPrivilege(PrivilegeHandler.PRIVILEGE_ACTION);
		assertFalse(privilegeAction.isAllAllowed());
		assertEquals(5, privilegeAction.getAllowList().size());
		assertEquals(0, privilegeAction.getDenyList().size());
		assertEquals("DefaultPrivilege", privilegeAction.getPolicy());

		IPrivilege privilegeAddRole = privilegeAdmin.getPrivilege(PrivilegeHandler.PRIVILEGE_ADD_ROLE);
		assertTrue(privilegeAddRole.isAllAllowed());
		assertEquals(0, privilegeAddRole.getAllowList().size());
		assertEquals(0, privilegeAddRole.getDenyList().size());

		IPrivilege privilegeRemRoleFromUser = privilegeAdmin.getPrivilege(
				PrivilegeHandler.PRIVILEGE_REMOVE_ROLE_FROM_USER);
		assertTrue(privilegeRemRoleFromUser.isAllAllowed());
		assertEquals(0, privilegeRemRoleFromUser.getAllowList().size());
		assertEquals(0, privilegeRemRoleFromUser.getDenyList().size());

		// AppUser
		Role appUser = findRole("AppUser", roles);
		assertEquals("AppUser", appUser.getName());
		assertEquals(new HashSet<>(Collections.singletonList("li.strolch.privilege.test.model.TestRestrictable")),
				appUser.getPrivilegeNames());

		IPrivilege testRestrictable = appUser.getPrivilege("li.strolch.privilege.test.model.TestRestrictable");
		assertEquals("li.strolch.privilege.test.model.TestRestrictable", testRestrictable.getName());
		assertEquals("DefaultPrivilege", testRestrictable.getPolicy());
		assertTrue(testRestrictable.isAllAllowed());
		assertEquals(0, testRestrictable.getAllowList().size());
		assertEquals(0, testRestrictable.getDenyList().size());

		// system_admin_privileges
		Role systemAdminPrivileges = findRole("system_admin_privileges", roles);
		assertEquals("system_admin_privileges", systemAdminPrivileges.getName());
		assertEquals(2, systemAdminPrivileges.getPrivilegeNames().size());
		MatcherAssert.assertThat(systemAdminPrivileges.getPrivilegeNames(),
				containsInAnyOrder("li.strolch.privilege.handler.SystemAction",
						"li.strolch.privilege.test.model.TestSystemRestrictable"));

		IPrivilege testSystemUserAction = systemAdminPrivileges.getPrivilege(
				"li.strolch.privilege.handler.SystemAction");
		assertEquals("li.strolch.privilege.handler.SystemAction", testSystemUserAction.getName());
		assertEquals("DefaultPrivilege", testSystemUserAction.getPolicy());
		assertFalse(testSystemUserAction.isAllAllowed());
		assertEquals(1, testSystemUserAction.getAllowList().size());
		assertEquals(1, testSystemUserAction.getDenyList().size());

		IPrivilege testSystemRestrictable = systemAdminPrivileges.getPrivilege(
				"li.strolch.privilege.test.model.TestSystemRestrictable");
		assertEquals("li.strolch.privilege.test.model.TestSystemRestrictable", testSystemRestrictable.getName());
		assertEquals("DefaultPrivilege", testSystemRestrictable.getPolicy());
		assertTrue(testSystemRestrictable.isAllAllowed());
		assertEquals(0, testSystemRestrictable.getAllowList().size());
		assertEquals(0, testSystemRestrictable.getDenyList().size());

		// restrictedRole
		Role restrictedRole = findRole("restrictedRole", roles);
		assertEquals("restrictedRole", restrictedRole.getName());
		assertEquals(1, restrictedRole.getPrivilegeNames().size());
		MatcherAssert.assertThat(restrictedRole.getPrivilegeNames(),
				containsInAnyOrder("li.strolch.privilege.handler.SystemAction"));

		IPrivilege testSystemUserAction2 = restrictedRole.getPrivilege("li.strolch.privilege.handler.SystemAction");
		assertEquals("li.strolch.privilege.handler.SystemAction", testSystemUserAction2.getName());
		assertEquals("DefaultPrivilege", testSystemUserAction2.getPolicy());
		assertFalse(testSystemUserAction2.isAllAllowed());
		assertEquals(1, testSystemUserAction2.getAllowList().size());
		assertEquals(1, testSystemUserAction2.getDenyList().size());
		MatcherAssert.assertThat(testSystemUserAction2.getAllowList(), containsInAnyOrder("hello"));
		MatcherAssert.assertThat(testSystemUserAction2.getDenyList(), containsInAnyOrder("goodbye"));
	}

	private User findUser(String username, Map<String, User> users) {
		return Optional.ofNullable(users.get(username))
				.orElseThrow(() -> new IllegalStateException("User " + username + " does not exist!"));
	}

	private Role findRole(String name, Map<String, Role> roles) {
		return Optional.ofNullable(roles.get(name))
				.orElseThrow(() -> new IllegalStateException("Role " + name + " does not exist!"));
	}

	@Test
	public void canWriteUsers() {

		Map<String, String> propertyMap;
		Set<String> userRoles;

		List<User> users = new ArrayList<>();
		propertyMap = new HashMap<>();
		propertyMap.put("prop1", "value1");
		userRoles = new HashSet<>();
		userRoles.add("role1");
		UserHistory history = new UserHistory();
		history.setFirstLogin(ZonedDateTime.of(LocalDateTime.of(2020, 1, 2, 2, 3, 4, 5), ZoneId.systemDefault()));
		User user1 = new User("1", "user1", "blabla".getBytes(), "blabla".getBytes(), "PBKDF2WithHmacSHA512", 10000,
				256, "Bob", "White", UserState.DISABLED, userRoles, Locale.ENGLISH, propertyMap, false, history);
		users.add(user1);

		propertyMap = new HashMap<>();
		propertyMap.put("prop2", "value2");
		userRoles = new HashSet<>();
		userRoles.add("role2");
		history = new UserHistory();
		history.setFirstLogin(ZonedDateTime.of(LocalDateTime.of(2020, 1, 2, 2, 3, 4, 5), ZoneId.systemDefault()));
		history.setLastLogin(ZonedDateTime.of(LocalDateTime.of(2020, 1, 5, 2, 3, 4, 5), ZoneId.systemDefault()));
		User user2 = new User("2", "user2", "haha".getBytes(), "haha".getBytes(), null, -1, -1, "Leonard", "Sheldon",
				UserState.ENABLED, userRoles, Locale.ENGLISH, propertyMap, false, history);
		users.add(user2);

		File modelFile = new File(TARGET_TEST + "PrivilegeUsersTest.xml");
		PrivilegeUsersDomWriter configSaxWriter = new PrivilegeUsersDomWriter(users, modelFile);
		configSaxWriter.write();

		PrivilegeUsersSaxReader xmlHandler = new PrivilegeUsersSaxReader(true);
		XmlHelper.parseDocument(modelFile, xmlHandler);

		Map<String, User> parsedUsers = xmlHandler.getUsers();
		assertNotNull(parsedUsers);
		assertEquals(2, parsedUsers.size());

		User parsedUser1 = parsedUsers.values()
				.stream()
				.filter(u -> u.getUsername().equals("user1"))
				.findAny()
				.orElseThrow(() -> new RuntimeException("user1 missing!"));
		User parsedUser2 = parsedUsers.values()
				.stream()
				.filter(u -> u.getUsername().equals("user2"))
				.findAny()
				.orElseThrow(() -> new RuntimeException("user2 missing!"));

		assertEquals(user1.getFirstname(), parsedUser1.getFirstname());
		assertEquals(user1.getLastname(), parsedUser1.getLastname());
		assertEquals(user1.getLocale(), parsedUser1.getLocale());
		assertArrayEquals(user1.getPassword(), parsedUser1.getPassword());
		assertArrayEquals(user1.getSalt(), parsedUser1.getSalt());
		assertEquals(user1.getProperties(), parsedUser1.getProperties());
		assertEquals(user1.getUserId(), parsedUser1.getUserId());
		assertEquals(user1.getUserState(), parsedUser1.getUserState());
		assertEquals(user1.getRoles(), parsedUser1.getRoles());

		assertEquals(user2.getFirstname(), parsedUser2.getFirstname());
		assertEquals(user2.getLastname(), parsedUser2.getLastname());
		assertEquals(user2.getLocale(), parsedUser2.getLocale());
		assertArrayEquals(user2.getPassword(), parsedUser2.getPassword());
		assertArrayEquals(user2.getSalt(), parsedUser2.getSalt());
		assertEquals(user2.getProperties(), parsedUser2.getProperties());
		assertEquals(user2.getUserId(), parsedUser2.getUserId());
		assertEquals(user2.getUserState(), parsedUser2.getUserState());
		assertEquals(user2.getRoles(), parsedUser2.getRoles());
	}

	@Test
	public void canWriteRoles() {

		Map<String, IPrivilege> privilegeMap;
		List<Role> roles = new ArrayList<>();
		Set<String> list = Collections.emptySet();
		privilegeMap = new HashMap<>();
		privilegeMap.put("priv1", new PrivilegeImpl("priv1", "DefaultPrivilege", true, list, list));
		Role role1 = new Role("role1", privilegeMap);
		roles.add(role1);

		privilegeMap = new HashMap<>();
		Set<String> denyList = new HashSet<>();
		denyList.add("myself");
		Set<String> allowList = new HashSet<>();
		allowList.add("other");
		privilegeMap.put("priv2", new PrivilegeImpl("priv2", "DefaultPrivilege", false, denyList, allowList));
		Role role2 = new Role("role2", privilegeMap);
		roles.add(role2);

		File modelFile = new File(TARGET_TEST + "PrivilegeRolesTest.xml");
		PrivilegeRolesDomWriter configSaxWriter = new PrivilegeRolesDomWriter(roles, modelFile);
		configSaxWriter.write();

		PrivilegeRolesSaxReader xmlHandler = new PrivilegeRolesSaxReader();
		XmlHelper.parseDocument(modelFile, xmlHandler);

		Map<String, Role> parsedRoles = xmlHandler.getRoles();
		assertNotNull(parsedRoles);
		assertEquals(2, parsedRoles.size());

		assertEquals(2, parsedRoles.size());
		Role parsedRole1 = parsedRoles.values()
				.stream()
				.filter(r -> r.getName().equals("role1"))
				.findAny()
				.orElseThrow(() -> new RuntimeException("role1 missing!"));
		Role parsedRole2 = parsedRoles.values()
				.stream()
				.filter(r -> r.getName().equals("role2"))
				.findAny()
				.orElseThrow(() -> new RuntimeException("role2 missing!"));

		Set<String> privilegeNames = role1.getPrivilegeNames();
		assertEquals(privilegeNames, parsedRole1.getPrivilegeNames());
		for (String privilegeName : privilegeNames) {
			IPrivilege privilege = role1.getPrivilege(privilegeName);
			IPrivilege privilege2 = parsedRole1.getPrivilege(privilegeName);
			assertNotNull(privilege);
			assertNotNull(privilege2);

			assertEquals(privilege.isAllAllowed(), privilege2.isAllAllowed());
			assertEquals(privilege.getAllowList(), privilege2.getAllowList());
			assertEquals(privilege.getDenyList(), privilege2.getDenyList());
			assertEquals(privilege.getName(), privilege2.getName());
			assertEquals(privilege.getPolicy(), privilege2.getPolicy());
		}

		assertEquals(role2.getPrivilegeNames(), parsedRole2.getPrivilegeNames());
		privilegeNames = role2.getPrivilegeNames();
		for (String privilegeName : privilegeNames) {
			IPrivilege privilege = role2.getPrivilege(privilegeName);
			IPrivilege privilege2 = parsedRole2.getPrivilege(privilegeName);
			assertNotNull(privilege);
			assertNotNull(privilege2);

			assertEquals(privilege.isAllAllowed(), privilege2.isAllAllowed());
			assertEquals(privilege.getAllowList(), privilege2.getAllowList());
			assertEquals(privilege.getDenyList(), privilege2.getDenyList());
			assertEquals(privilege.getName(), privilege2.getName());
			assertEquals(privilege.getPolicy(), privilege2.getPolicy());
		}
	}
}
