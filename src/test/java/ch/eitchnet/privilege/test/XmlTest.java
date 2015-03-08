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
package ch.eitchnet.privilege.test;

import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.handler.DefaultEncryptionHandler;
import ch.eitchnet.privilege.handler.XmlPersistenceHandler;
import ch.eitchnet.privilege.model.IPrivilege;
import ch.eitchnet.privilege.model.UserState;
import ch.eitchnet.privilege.model.internal.PrivilegeContainerModel;
import ch.eitchnet.privilege.model.internal.PrivilegeImpl;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.User;
import ch.eitchnet.privilege.xml.PrivilegeConfigDomWriter;
import ch.eitchnet.privilege.xml.PrivilegeConfigSaxReader;
import ch.eitchnet.privilege.xml.PrivilegeModelDomWriter;
import ch.eitchnet.privilege.xml.PrivilegeModelSaxReader;
import ch.eitchnet.utils.helper.FileHelper;
import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.utils.helper.XmlHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class XmlTest {

	private static final String TARGET_TEST = "./target/test";
	private static final Logger logger = LoggerFactory.getLogger(XmlTest.class);

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@BeforeClass
	public static void init() throws Exception {

		cleanUp();

		File tmpDir = new File("target/test");
		if (tmpDir.exists())
			FileHelper.deleteFile(tmpDir, false);
		tmpDir.mkdirs();
	}

	@AfterClass
	public static void cleanUp() throws Exception {

		File tmpDir = new File("target/test");
		if (!tmpDir.exists())
			return;

		File tmpFile = new File("target/test/PrivilegeTest.xml");
		if (tmpFile.exists() && !tmpFile.delete()) {
			throw new RuntimeException("Tmp still exists and can not be deleted at " + tmpFile.getAbsolutePath());
		}

		tmpFile = new File("target/test/PrivilegeModelTest.xml");
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
		File xmlFile = new File("config/Privilege.xml");
		XmlHelper.parseDocument(xmlFile, saxReader);
		logger.info(containerModel.toString());

		// assert all objects read
		assertNotNull(containerModel.getParameterMap());
		assertNotNull(containerModel.getPolicies());
		assertNotNull(containerModel.getEncryptionHandlerClassName());
		assertNotNull(containerModel.getEncryptionHandlerParameterMap());
		assertNotNull(containerModel.getPersistenceHandlerClassName());
		assertNotNull(containerModel.getPersistenceHandlerParameterMap());

		assertEquals(1, containerModel.getParameterMap().size());
		assertEquals(1, containerModel.getPolicies().size());
		assertEquals(1, containerModel.getEncryptionHandlerParameterMap().size());
		assertEquals(2, containerModel.getPersistenceHandlerParameterMap().size());

		// TODO extend assertions to actual model
	}

	@Test
	public void canWriteConfig() {

		Map<String, String> parameterMap = new HashMap<String, String>();
		Map<String, String> encryptionHandlerParameterMap = new HashMap<String, String>();
		Map<String, String> persistenceHandlerParameterMap = new HashMap<String, String>();

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

		containerModel.addPolicy("DefaultPrivilege", "ch.eitchnet.privilege.policy.DefaultPrivilege");

		File configFile = new File("./target/test/PrivilegeTest.xml");
		PrivilegeConfigDomWriter configSaxWriter = new PrivilegeConfigDomWriter(containerModel, configFile);
		configSaxWriter.write();

		String fileHash = StringHelper.getHexString(FileHelper.hashFileSha256(configFile));
		assertEquals("2abd3442eec8bcec5bee365aab6db2fd4e1789325425cb1e017e900582525685", fileHash);
	}

	@Test
	public void canReadModel() {

		PrivilegeModelSaxReader xmlHandler = new PrivilegeModelSaxReader();
		File xmlFile = new File("config/PrivilegeModel.xml");
		XmlHelper.parseDocument(xmlFile, xmlHandler);

		List<User> users = xmlHandler.getUsers();
		assertNotNull(users);
		List<Role> roles = xmlHandler.getRoles();
		assertNotNull(roles);

		assertEquals(3, users.size());
		assertEquals(6, roles.size());

		// assert model

		//
		// users
		//

		// admin
		User admin = findUser("admin", users);
		assertEquals("1", admin.getUserId());
		assertEquals("admin", admin.getUsername());
		assertEquals("8c6976e5b5410415bde908bd4dee15dfb167a9c873fc4bb8a81f6f2ab448a918", admin.getPassword());
		assertEquals("Application", admin.getFirstname());
		assertEquals("Administrator", admin.getLastname());
		assertEquals(UserState.ENABLED, admin.getUserState());
		assertEquals("en_gb", admin.getLocale().toString());
		assertThat(admin.getRoles(), containsInAnyOrder("PrivilegeAdmin", "AppUser"));
		Map<String, String> properties = admin.getProperties();
		assertEquals(new HashSet<String>(Arrays.asList("organization", "organizationalUnit")), properties.keySet());
		assertEquals("eitchnet.ch", properties.get("organization"));
		assertEquals("Development", properties.get("organizationalUnit"));

		// system_admin
		User systemAdmin = findUser("system_admin", users);
		assertEquals("2", systemAdmin.getUserId());
		assertEquals("system_admin", systemAdmin.getUsername());
		assertEquals(null, systemAdmin.getPassword());
		assertEquals("System User", systemAdmin.getFirstname());
		assertEquals("Administrator", systemAdmin.getLastname());
		assertEquals(UserState.SYSTEM, systemAdmin.getUserState());
		assertEquals("en_gb", systemAdmin.getLocale().toString());
		assertThat(systemAdmin.getRoles(), containsInAnyOrder("system_admin_privileges"));
		assertTrue(systemAdmin.getProperties().isEmpty());

		//
		// roles
		//

		// PrivilegeAdmin
		Role privilegeAdmin = findRole("PrivilegeAdmin", roles);
		assertEquals("PrivilegeAdmin", privilegeAdmin.getName());
		assertTrue(privilegeAdmin.getPrivilegeNames().isEmpty());

		// AppUser
		Role appUser = findRole("AppUser", roles);
		assertEquals("AppUser", appUser.getName());
		assertEquals(new HashSet<String>(Arrays.asList("ch.eitchnet.privilege.test.model.TestRestrictable")),
				appUser.getPrivilegeNames());

		IPrivilege testRestrictable = appUser.getPrivilege("ch.eitchnet.privilege.test.model.TestRestrictable");
		assertEquals("ch.eitchnet.privilege.test.model.TestRestrictable", testRestrictable.getName());
		assertEquals("DefaultPrivilege", testRestrictable.getPolicy());
		assertTrue(testRestrictable.isAllAllowed());
		assertEquals(0, testRestrictable.getAllowList().size());
		assertEquals(0, testRestrictable.getDenyList().size());

		// system_admin_privileges
		Role systemAdminPrivileges = findRole("system_admin_privileges", roles);
		assertEquals("system_admin_privileges", systemAdminPrivileges.getName());
		assertEquals(2, systemAdminPrivileges.getPrivilegeNames().size());
		assertThat(
				systemAdminPrivileges.getPrivilegeNames(),
				containsInAnyOrder("ch.eitchnet.privilege.test.model.TestSystemUserAction",
						"ch.eitchnet.privilege.test.model.TestSystemRestrictable"));

		IPrivilege testSystemUserAction = systemAdminPrivileges
				.getPrivilege("ch.eitchnet.privilege.test.model.TestSystemUserAction");
		assertEquals("ch.eitchnet.privilege.test.model.TestSystemUserAction", testSystemUserAction.getName());
		assertEquals("DefaultPrivilege", testSystemUserAction.getPolicy());
		assertTrue(testSystemUserAction.isAllAllowed());
		assertEquals(0, testSystemUserAction.getAllowList().size());
		assertEquals(0, testSystemUserAction.getDenyList().size());

		IPrivilege testSystemRestrictable = systemAdminPrivileges
				.getPrivilege("ch.eitchnet.privilege.test.model.TestSystemRestrictable");
		assertEquals("ch.eitchnet.privilege.test.model.TestSystemRestrictable", testSystemRestrictable.getName());
		assertEquals("DefaultPrivilege", testSystemRestrictable.getPolicy());
		assertTrue(testSystemRestrictable.isAllAllowed());
		assertEquals(0, testSystemRestrictable.getAllowList().size());
		assertEquals(0, testSystemRestrictable.getDenyList().size());

		// restrictedRole
		Role restrictedRole = findRole("restrictedRole", roles);
		assertEquals("restrictedRole", restrictedRole.getName());
		assertEquals(1, restrictedRole.getPrivilegeNames().size());
		assertThat(restrictedRole.getPrivilegeNames(),
				containsInAnyOrder("ch.eitchnet.privilege.test.model.TestSystemUserAction"));

		IPrivilege testSystemUserAction2 = restrictedRole
				.getPrivilege("ch.eitchnet.privilege.test.model.TestSystemUserAction");
		assertEquals("ch.eitchnet.privilege.test.model.TestSystemUserAction", testSystemUserAction2.getName());
		assertEquals("DefaultPrivilege", testSystemUserAction2.getPolicy());
		assertFalse(testSystemUserAction2.isAllAllowed());
		assertEquals(1, testSystemUserAction2.getAllowList().size());
		assertEquals(1, testSystemUserAction2.getDenyList().size());
		assertThat(testSystemUserAction2.getAllowList(), containsInAnyOrder("hello"));
		assertThat(testSystemUserAction2.getDenyList(), containsInAnyOrder("goodbye"));
	}

	/**
	 * @param username
	 * @param users
	 * @return
	 */
	private User findUser(String username, List<User> users) {
		for (User user : users) {
			if (user.getUsername().equals(username))
				return user;
		}

		throw new RuntimeException("No user exists with username " + username);
	}

	/**
	 * @param name
	 * @param roles
	 * @return
	 */
	private Role findRole(String name, List<Role> roles) {
		for (Role role : roles) {
			if (role.getName().equals(name))
				return role;
		}

		throw new RuntimeException("No role exists with name " + name);
	}

	@Test
	public void canWriteModel() {

		Map<String, String> propertyMap;
		Set<String> userRoles;
		Map<String, IPrivilege> privilegeMap;

		List<User> users = new ArrayList<User>();
		propertyMap = new HashMap<String, String>();
		propertyMap.put("prop1", "value1");
		userRoles = new HashSet<String>();
		userRoles.add("role1");
		users.add(new User("1", "user1", "blabla", "Bob", "White", UserState.DISABLED, userRoles, Locale.ENGLISH,
				propertyMap));

		propertyMap = new HashMap<String, String>();
		propertyMap.put("prop2", "value2");
		userRoles = new HashSet<String>();
		userRoles.add("role2");
		users.add(new User("2", "user2", "haha", "Leonard", "Sheldon", UserState.ENABLED, userRoles, Locale.ENGLISH,
				propertyMap));

		List<Role> roles = new ArrayList<Role>();
		Set<String> list = Collections.emptySet();
		privilegeMap = new HashMap<String, IPrivilege>();
		privilegeMap.put("priv1", new PrivilegeImpl("priv1", "DefaultPrivilege", true, list, list));
		roles.add(new Role("role1", privilegeMap));

		privilegeMap = new HashMap<String, IPrivilege>();
		Set<String> denyList = new HashSet<String>();
		denyList.add("myself");
		Set<String> allowList = new HashSet<String>();
		allowList.add("other");
		privilegeMap.put("priv2", new PrivilegeImpl("priv2", "DefaultPrivilege", false, denyList, allowList));
		roles.add(new Role("role2", privilegeMap));

		File modelFile = new File("./target/test/PrivilegeModelTest.xml");
		PrivilegeModelDomWriter configSaxWriter = new PrivilegeModelDomWriter(users, roles, modelFile);
		configSaxWriter.write();

		String fileHash = StringHelper.getHexString(FileHelper.hashFileSha256(modelFile));
		assertEquals("c9732a05bf0ed53d89b3d12e7c8d7216150b6a91412d1bf47fbe3e6f3be750ff", fileHash);
	}
}
