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

import li.strolch.privilege.handler.*;
import li.strolch.privilege.model.Group;
import li.strolch.privilege.model.Privilege;
import li.strolch.privilege.model.UserState;
import li.strolch.privilege.model.internal.*;
import li.strolch.privilege.test.model.DummySsoHandler;
import li.strolch.privilege.xml.*;
import li.strolch.utils.helper.FileHelper;
import li.strolch.utils.helper.XmlHelper;
import org.hamcrest.MatcherAssert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.xml.stream.XMLStreamException;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.*;

import static li.strolch.utils.helper.StringHelper.toHexString;
import static li.strolch.utils.iso8601.ISO8601.parseToZdt;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.junit.Assert.*;

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

		tmpFile = new File(TARGET_TEST + "PrivilegeGroupsTest.xml");
		if (tmpFile.exists() && !tmpFile.delete()) {
			throw new RuntimeException("Tmp still exists and can not be deleted at " + tmpFile.getAbsolutePath());
		}

		tmpFile = new File(TARGET_TEST + "PrivilegeRolesTest.xml");
		if (tmpFile.exists() && !tmpFile.delete()) {
			throw new RuntimeException("Tmp still exists and can not be deleted at " + tmpFile.getAbsolutePath());
		}

		tmpFile = new File(TARGET_TEST + "PrivilegeTokensTest.xml");
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

		PrivilegeContainerModel containerModel = new PrivilegeContainerModel(new File(SRC_TEST));
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
		assertEquals(4, containerModel.getPersistenceHandlerParameterMap().size());
	}

	@Test
	public void canWriteConfig() throws XMLStreamException, IOException {

		Map<String, String> parameterMap = new HashMap<>();
		Map<String, String> encryptionHandlerParameterMap = new HashMap<>();
		Map<String, String> persistenceHandlerParameterMap = new HashMap<>();

		parameterMap.put("autoPersistOnPasswordChange", "true");
		encryptionHandlerParameterMap.put("hashAlgorithm", "SHA-256");
		persistenceHandlerParameterMap.put("modelXmlFile", "PrivilegeModel.xml");

		PrivilegeContainerModel containerModel = new PrivilegeContainerModel(new File(TARGET_TEST));
		containerModel.setParameterMap(parameterMap);
		containerModel.setPrivilegeHandlerClassName(DefaultPrivilegeHandler.class.getName());
		containerModel.setEncryptionHandlerClassName(DefaultEncryptionHandler.class.getName());
		containerModel.setEncryptionHandlerParameterMap(encryptionHandlerParameterMap);
		containerModel.setPersistenceHandlerClassName(XmlPersistenceHandler.class.getName());
		containerModel.setPersistenceHandlerParameterMap(persistenceHandlerParameterMap);
		containerModel.setUserChallengeHandlerClassName(MailUserChallengeHandler.class.getName());
		containerModel.setSsoHandlerClassName(DummySsoHandler.class.getName());

		containerModel.addPolicy("DefaultPrivilege", "li.strolch.privilege.policy.DefaultPrivilege");

		File configFile = new File(TARGET_TEST + "PrivilegeTest.xml");
		PrivilegeConfigSaxWriter configSaxWriter = new PrivilegeConfigSaxWriter(containerModel, configFile);
		configSaxWriter.write();

		String expected = """
				<?xml version="1.0" encoding="UTF-8"?>
				<Privilege>
				    <Container>
				        <Parameters>
				            <Parameter name="autoPersistOnPasswordChange" value="true"/>
				        </Parameters>
				        <PrivilegeHandler class="li.strolch.privilege.handler.DefaultPrivilegeHandler"/>
				        <EncryptionHandler class="li.strolch.privilege.handler.DefaultEncryptionHandler">
				            <Parameters>
				                <Parameter name="hashAlgorithm" value="SHA-256"/>
				            </Parameters>
				        </EncryptionHandler>
				        <PersistenceHandler class="li.strolch.privilege.handler.XmlPersistenceHandler">
				            <Parameters>
				                <Parameter name="modelXmlFile" value="PrivilegeModel.xml"/>
				            </Parameters>
				        </PersistenceHandler>
				        <UserChallengeHandler class="li.strolch.privilege.handler.MailUserChallengeHandler"/>
				        <SsoHandler class="li.strolch.privilege.test.model.DummySsoHandler"/>
				    </Container>
				    <Policies>
				        <Policy name="DefaultPrivilege" class="li.strolch.privilege.policy.DefaultPrivilege"/>
				    </Policies>
				</Privilege>
				""";

		assertEquals(expected, Files.readString(configFile.toPath()));
	}

	@Test
	public void canReadUsers() {

		PrivilegeUsersSaxReader xmlHandler = new PrivilegeUsersSaxReader(true, true);
		File xmlFile = new File(SRC_TEST + "PrivilegeUsers.xml");
		XmlHelper.parseDocument(xmlFile, xmlHandler);

		Map<String, User> users = xmlHandler.getUsersByUsername();
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
				toHexString(admin.getPasswordCrypt().password()));
		assertEquals("61646d696e", toHexString(admin.getPasswordCrypt().salt()));
		assertEquals("Application", admin.getFirstname());
		assertEquals("Administrator", admin.getLastname());
		assertEquals(UserState.ENABLED, admin.getUserState());
		assertEquals("en-GB", admin.getLocale().toLanguageTag());
		assertEquals(Set.of("GroupA"), admin.getGroups());
		MatcherAssert.assertThat(admin.getRoles(), containsInAnyOrder("PrivilegeAdmin", "AppUser"));
		Map<String, String> properties = admin.getProperties();
		assertEquals(new HashSet<>(Arrays.asList("organization", "organizationalUnit")), properties.keySet());
		assertEquals("eitchnet.ch", properties.get("organization"));
		assertEquals("Development", properties.get("organizationalUnit"));

		// system_admin
		User systemAdmin = findUser("system_admin", users);
		assertEquals("3", systemAdmin.getUserId());
		assertEquals("system_admin", systemAdmin.getUsername());
		assertNull(systemAdmin.getPasswordCrypt());
		assertEquals("System User", systemAdmin.getFirstname());
		assertEquals("Administrator", systemAdmin.getLastname());
		assertEquals(UserState.SYSTEM, systemAdmin.getUserState());
		assertEquals("en-GB", systemAdmin.getLocale().toLanguageTag());
		assertEquals(Set.of(), systemAdmin.getGroups());
		MatcherAssert.assertThat(systemAdmin.getRoles(), containsInAnyOrder("system_admin_privileges"));
		assertTrue(systemAdmin.getProperties().isEmpty());

		// admin2
		User admin2 = findUser("admin2", users);
		assertEquals("2", admin2.getUserId());
		assertEquals("admin2", admin2.getUsername());
		assertEquals("8c6976e5b5410415bde908bd4dee15dfb167a9c873fc4bb8a81f6f2ab448a918",
				toHexString(admin2.getPasswordCrypt().password()));
		assertEquals("Application", admin2.getFirstname());
		assertEquals("Administrator", admin2.getLastname());
		assertEquals(UserState.ENABLED, admin2.getUserState());
		assertEquals("en-GB", admin2.getLocale().toLanguageTag());
		MatcherAssert.assertThat(admin2.getGroups(), containsInAnyOrder("AppUserLocationA"));
		MatcherAssert.assertThat(admin2.getRoles(), containsInAnyOrder("PrivilegeAdmin"));
		properties = admin2.getProperties();
		assertEquals(new HashSet<>(Arrays.asList("organization", "organizationalUnit")), properties.keySet());
		assertEquals("eitchnet.ch", properties.get("organization"));
		assertEquals("Development", properties.get("organizationalUnit"));

	}

	@Test
	public void canReadGroups() {

		PrivilegeGroupsSaxReader xmlHandler = new PrivilegeGroupsSaxReader(true);
		File xmlFile = new File(SRC_TEST + "PrivilegeGroups.xml");
		XmlHelper.parseDocument(xmlFile, xmlHandler);

		Map<String, Group> groups = xmlHandler.getGroups();
		assertNotNull(groups);

		assertEquals(3, groups.size());

		// group AppUserLocationA
		Group group = groups.get("AppUserLocationA");
		assertEquals("AppUserLocationA", group.name());
		MatcherAssert.assertThat(group.roles(), containsInAnyOrder("AppUser", "MyRole"));
		Map<String, String> properties = group.getProperties();
		assertEquals(new HashSet<>(List.of("location")), properties.keySet());
		assertEquals("LocationA", properties.get("location"));

		group = groups.get("GroupA");
		assertEquals("GroupA", group.name());
		properties = group.getProperties();
		assertTrue(properties.isEmpty());
		assertTrue(group.roles().isEmpty());
	}

	@Test
	public void canReadTokens() {

		PrivilegeTokensSaxReader xmlHandler = new PrivilegeTokensSaxReader(true);
		File xmlFile = new File(SRC_TEST + "PrivilegeTokens.xml");
		XmlHelper.parseDocument(xmlFile, xmlHandler);

		Map<String, AccessToken> tokens = xmlHandler.getTokens();
		assertNotNull(tokens);

		assertEquals(1, tokens.size());

		AccessToken token = tokens.get("50b31270-bc49-4940-97ec-d4aa0d1ad649");
		assertEquals("50b31270-bc49-4940-97ec-d4aa0d1ad649", token.tokenId());
		assertEquals("admin", token.username());
		assertEquals("cb69962946617da006a2f95776d78b49e5ec7941d2bdb2d25cdb05f957f64344",
				toHexString(token.passwordCrypt().password()));
		assertEquals(parseToZdt("2024-02-12T08:00:00.000+01:00"), token.validFrom());
		assertEquals(parseToZdt("3000-01-01T01:00:00.000+01:00"), token.validTo());
		assertEquals(2, token.privileges().size());

		Privilege privilegeAction = token.privileges().get("Foo");
		assertFalse(privilegeAction.isAllAllowed());
		assertEquals(1, privilegeAction.getAllowList().size());
		assertEquals(1, privilegeAction.getDenyList().size());
		assertEquals("DefaultPrivilege", privilegeAction.getPolicy());
	}

	@Test
	public void canReadRoles() {

		PrivilegeRolesSaxReader xmlHandler = new PrivilegeRolesSaxReader(true);
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
		assertEquals(22, privilegeAdmin.getPrivilegeNames().size());
		Privilege privilegeAction = privilegeAdmin.getPrivilege(PrivilegeHandler.PRIVILEGE_ACTION);
		assertFalse(privilegeAction.isAllAllowed());
		assertEquals(5, privilegeAction.getAllowList().size());
		assertEquals(0, privilegeAction.getDenyList().size());
		assertEquals("DefaultPrivilege", privilegeAction.getPolicy());

		Privilege privilegeAddRole = privilegeAdmin.getPrivilege(PrivilegeHandler.PRIVILEGE_ADD_ROLE);
		assertTrue(privilegeAddRole.isAllAllowed());
		assertEquals(0, privilegeAddRole.getAllowList().size());
		assertEquals(0, privilegeAddRole.getDenyList().size());

		Privilege privilegeRemRole = privilegeAdmin.getPrivilege(PrivilegeHandler.PRIVILEGE_REMOVE_ROLE);
		assertTrue(privilegeRemRole.isAllAllowed());
		assertEquals(0, privilegeRemRole.getAllowList().size());
		assertEquals(0, privilegeRemRole.getDenyList().size());

		// AppUser
		Role appUser = findRole("AppUser", roles);
		assertEquals("AppUser", appUser.getName());
		assertEquals(new HashSet<>(Collections.singletonList("li.strolch.privilege.test.model.TestRestrictable")),
				appUser.getPrivilegeNames());

		Privilege testRestrictable = appUser.getPrivilege("li.strolch.privilege.test.model.TestRestrictable");
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

		Privilege testSystemUserAction = systemAdminPrivileges.getPrivilege(
				"li.strolch.privilege.handler.SystemAction");
		assertEquals("li.strolch.privilege.handler.SystemAction", testSystemUserAction.getName());
		assertEquals("DefaultPrivilege", testSystemUserAction.getPolicy());
		assertFalse(testSystemUserAction.isAllAllowed());
		assertEquals(1, testSystemUserAction.getAllowList().size());
		assertEquals(1, testSystemUserAction.getDenyList().size());

		Privilege testSystemRestrictable = systemAdminPrivileges.getPrivilege(
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

		Privilege testSystemUserAction2 = restrictedRole.getPrivilege("li.strolch.privilege.handler.SystemAction");
		assertEquals("li.strolch.privilege.handler.SystemAction", testSystemUserAction2.getName());
		assertEquals("DefaultPrivilege", testSystemUserAction2.getPolicy());
		assertFalse(testSystemUserAction2.isAllAllowed());
		assertEquals(1, testSystemUserAction2.getAllowList().size());
		assertEquals(1, testSystemUserAction2.getDenyList().size());
		MatcherAssert.assertThat(testSystemUserAction2.getAllowList(), containsInAnyOrder("hello"));
		MatcherAssert.assertThat(testSystemUserAction2.getDenyList(), containsInAnyOrder("goodbye"));
	}

	private User findUser(String username, Map<String, User> users) {
		return Optional
				.ofNullable(users.get(username))
				.orElseThrow(() -> new IllegalStateException("User " + username + " does not exist!"));
	}

	private Role findRole(String name, Map<String, Role> roles) {
		return Optional
				.ofNullable(roles.get(name))
				.orElseThrow(() -> new IllegalStateException("Role " + name + " does not exist!"));
	}

	@Test
	public void canWriteUsers() throws XMLStreamException, IOException {

		Map<String, String> propertyMap;
		Set<String> groups;
		Set<String> userRoles;

		List<User> users = new ArrayList<>();
		propertyMap = new HashMap<>();
		propertyMap.put("prop1", "value1");
		groups = new HashSet<>();
		groups.add("group1");
		userRoles = new HashSet<>();
		userRoles.add("role1");
		UserHistory history = UserHistory.EMPTY.withFirstLogin(
				ZonedDateTime.of(LocalDateTime.of(2020, 1, 2, 2, 3, 4, 5), ZoneId.systemDefault()));
		User user1 = new User("1", "user1",
				new PasswordCrypt("blabla".getBytes(), "blabla".getBytes(), "PBKDF2WithHmacSHA512", 10000, 256), "Bob",
				"White", UserState.DISABLED, groups, userRoles, Locale.ENGLISH, propertyMap, false, history);
		users.add(user1);

		propertyMap = new HashMap<>();
		propertyMap.put("prop2", "value2");
		groups = new HashSet<>();
		groups.add("group2");
		userRoles = new HashSet<>();
		userRoles.add("role2");
		history = UserHistory.EMPTY
				.withFirstLogin(ZonedDateTime.of(LocalDateTime.of(2020, 1, 2, 2, 3, 4, 5), ZoneId.systemDefault()))
				.withLastLogin(ZonedDateTime.of(LocalDateTime.of(2020, 1, 5, 2, 3, 4, 5), ZoneId.systemDefault()));
		User user2 = new User("2", "user2", new PasswordCrypt("haha".getBytes(), "haha".getBytes(), null, -1, -1),
				"Leonard", "Sheldon", UserState.ENABLED, groups, userRoles, Locale.ENGLISH, propertyMap, false,
				history);
		users.add(user2);

		File modelFile = new File(TARGET_TEST + "PrivilegeUsersTest.xml");
		PrivilegeUsersSaxWriter configSaxWriter = new PrivilegeUsersSaxWriter(users, modelFile);
		configSaxWriter.write();

		PrivilegeUsersSaxReader xmlHandler = new PrivilegeUsersSaxReader(true, true);
		XmlHelper.parseDocument(modelFile, xmlHandler);

		Map<String, User> parsedUsers = xmlHandler.getUsersByUsername();
		assertNotNull(parsedUsers);
		assertEquals(2, parsedUsers.size());

		User parsedUser1 = parsedUsers
				.values()
				.stream()
				.filter(u -> u.getUsername().equals("user1"))
				.findAny()
				.orElseThrow(() -> new RuntimeException("user1 missing!"));
		User parsedUser2 = parsedUsers
				.values()
				.stream()
				.filter(u -> u.getUsername().equals("user2"))
				.findAny()
				.orElseThrow(() -> new RuntimeException("user2 missing!"));

		assertEquals(user1.getFirstname(), parsedUser1.getFirstname());
		assertEquals(user1.getLastname(), parsedUser1.getLastname());
		assertEquals(user1.getLocale(), parsedUser1.getLocale());
		assertArrayEquals(user1.getPasswordCrypt().password(), parsedUser1.getPasswordCrypt().password());
		assertArrayEquals(user1.getPasswordCrypt().salt(), parsedUser1.getPasswordCrypt().salt());
		assertEquals(user1.getProperties(), parsedUser1.getProperties());
		assertEquals(user1.getUserId(), parsedUser1.getUserId());
		assertEquals(user1.getUserState(), parsedUser1.getUserState());
		assertEquals(user1.getRoles(), parsedUser1.getRoles());

		assertEquals(user2.getFirstname(), parsedUser2.getFirstname());
		assertEquals(user2.getLastname(), parsedUser2.getLastname());
		assertEquals(user2.getLocale(), parsedUser2.getLocale());
		assertArrayEquals(user2.getPasswordCrypt().password(), parsedUser2.getPasswordCrypt().password());
		assertArrayEquals(user2.getPasswordCrypt().salt(), parsedUser2.getPasswordCrypt().salt());
		assertEquals(user2.getProperties(), parsedUser2.getProperties());
		assertEquals(user2.getUserId(), parsedUser2.getUserId());
		assertEquals(user2.getUserState(), parsedUser2.getUserState());
		assertEquals(user2.getRoles(), parsedUser2.getRoles());
	}

	@Test
	public void canWriteGroups() throws XMLStreamException, IOException {

		Map<String, String> propertyMap;
		Set<String> roles;

		List<Group> groups = new ArrayList<>();
		propertyMap = new HashMap<>();
		propertyMap.put("prop1", "value1");
		roles = new HashSet<>();
		roles.add("role1");
		Group newGroup = new Group("group1", roles, propertyMap);
		groups.add(newGroup);

		File modelFile = new File(TARGET_TEST + "PrivilegeGroupsTest.xml");
		PrivilegeGroupsSaxWriter configSaxWriter = new PrivilegeGroupsSaxWriter(groups, modelFile);
		configSaxWriter.write();

		PrivilegeGroupsSaxReader xmlHandler = new PrivilegeGroupsSaxReader(true);
		XmlHelper.parseDocument(modelFile, xmlHandler);

		Map<String, Group> parsedGroups = xmlHandler.getGroups();
		assertNotNull(parsedGroups);
		assertEquals(1, parsedGroups.size());

		// group group1
		Group parsedGroup1 = parsedGroups.get("group1");
		assertNotNull(parsedGroup1);
		assertEquals("group1", parsedGroup1.name());
		MatcherAssert.assertThat(parsedGroup1.roles(), containsInAnyOrder("role1"));
		Map<String, String> properties = parsedGroup1.getProperties();
		assertEquals(new HashSet<>(List.of("prop1")), properties.keySet());
		assertEquals("value1", properties.get("prop1"));
	}

	@Test
	public void canWriteRoles() throws XMLStreamException, IOException {

		Map<String, Privilege> privilegeMap;
		List<Role> roles = new ArrayList<>();
		Set<String> list = Collections.emptySet();
		privilegeMap = new HashMap<>();
		privilegeMap.put("priv1", new Privilege("priv1", "DefaultPrivilege", true, list, list));
		Role role1 = new Role("role1", privilegeMap);
		roles.add(role1);

		privilegeMap = new HashMap<>();
		Set<String> denyList = new HashSet<>();
		denyList.add("myself");
		Set<String> allowList = new HashSet<>();
		allowList.add("other");
		privilegeMap.put("priv2", new Privilege("priv2", "DefaultPrivilege", false, denyList, allowList));
		Role role2 = new Role("role2", privilegeMap);
		roles.add(role2);

		File modelFile = new File(TARGET_TEST + "PrivilegeRolesTest.xml");
		PrivilegeRolesSaxWriter writer = new PrivilegeRolesSaxWriter(roles, modelFile);
		writer.write();

		PrivilegeRolesSaxReader xmlHandler = new PrivilegeRolesSaxReader(true);
		XmlHelper.parseDocument(modelFile, xmlHandler);

		Map<String, Role> parsedRoles = xmlHandler.getRoles();
		assertNotNull(parsedRoles);
		assertEquals(2, parsedRoles.size());

		assertEquals(2, parsedRoles.size());
		Role parsedRole1 = parsedRoles
				.values()
				.stream()
				.filter(r -> r.getName().equals("role1"))
				.findAny()
				.orElseThrow(() -> new RuntimeException("role1 missing!"));
		Role parsedRole2 = parsedRoles
				.values()
				.stream()
				.filter(r -> r.getName().equals("role2"))
				.findAny()
				.orElseThrow(() -> new RuntimeException("role2 missing!"));

		Set<String> privilegeNames = role1.getPrivilegeNames();
		assertEquals(privilegeNames, parsedRole1.getPrivilegeNames());
		for (String privilegeName : privilegeNames) {
			Privilege privilege = role1.getPrivilege(privilegeName);
			Privilege privilege2 = parsedRole1.getPrivilege(privilegeName);
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
			Privilege privilege = role2.getPrivilege(privilegeName);
			Privilege privilege2 = parsedRole2.getPrivilege(privilegeName);
			assertNotNull(privilege);
			assertNotNull(privilege2);

			assertEquals(privilege.isAllAllowed(), privilege2.isAllAllowed());
			assertEquals(privilege.getAllowList(), privilege2.getAllowList());
			assertEquals(privilege.getDenyList(), privilege2.getDenyList());
			assertEquals(privilege.getName(), privilege2.getName());
			assertEquals(privilege.getPolicy(), privilege2.getPolicy());
		}
	}

	@Test
	public void canWriteTokens() throws XMLStreamException, IOException {

		Map<String, Privilege> privileges1 = new HashMap<>();
		privileges1.put("Foo1",
				new Privilege("Foo1", "DefaultPrivilege", false, Set.of("allowFoo1"), Set.of("denyFoo1")));
		privileges1.put("Bar1",
				new Privilege("Bar1", "DefaultPrivilege", true, Collections.emptySet(), Collections.emptySet()));

		Map<String, Privilege> privileges2 = new HashMap<>();
		privileges2.put("Foo2",
				new Privilege("Foo2", "DefaultPrivilege", false, Set.of("allowFoo2"), Set.of("denyFoo2")));
		privileges2.put("Bar2",
				new Privilege("Bar2", "DefaultPrivilege", true, Collections.emptySet(), Collections.emptySet()));

		ZonedDateTime now = ZonedDateTime.now().truncatedTo(ChronoUnit.MILLIS);
		AccessToken token1 = new AccessToken(UUID.randomUUID().toString(), "admin",
				new PasswordCrypt("password1".getBytes(), "salt1".getBytes(), "PBKDF2WithHmacSHA512", 10000, 256), now,
				now.plusDays(1), privileges1);

		AccessToken token2 = new AccessToken(UUID.randomUUID().toString(), "user",
				new PasswordCrypt("password2".getBytes(), "salt2".getBytes(), "PBKDF2WithHmacSHA512", 10000, 256), now,
				now.plusDays(2), privileges2);

		List<AccessToken> tokens = new ArrayList<>();
		tokens.add(token1);
		tokens.add(token2);

		File tokensFile = new File(TARGET_TEST + "PrivilegeTokensTest.xml");
		PrivilegeTokensSaxWriter tokensSaxWriter = new PrivilegeTokensSaxWriter(tokens, tokensFile);
		tokensSaxWriter.write();

		PrivilegeTokensSaxReader xmlHandler = new PrivilegeTokensSaxReader(true);
		XmlHelper.parseDocument(tokensFile, xmlHandler);

		Map<String, AccessToken> parsedTokens = xmlHandler.getTokens();
		assertNotNull(parsedTokens);
		assertEquals(2, parsedTokens.size());

		AccessToken parsedToken1 = parsedTokens.get(token1.tokenId());
		AccessToken parsedToken2 = parsedTokens.get(token2.tokenId());

		assertEquals(token1.username(), parsedToken1.username());
		assertEquals(token1.validFrom(), parsedToken1.validFrom());
		assertEquals(token1.validTo(), parsedToken1.validTo());
		assertEquals(token1.passwordCrypt(), parsedToken1.passwordCrypt());
		assertEquals(token1.privileges(), parsedToken1.privileges());

		assertEquals(token2.username(), parsedToken2.username());
		assertEquals(token2.validFrom(), parsedToken2.validFrom());
		assertEquals(token2.validTo(), parsedToken2.validTo());
		assertEquals(token2.passwordCrypt(), parsedToken2.passwordCrypt());
		assertEquals(token2.privileges(), parsedToken2.privileges());
	}
}