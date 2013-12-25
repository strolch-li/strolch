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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.util.ArrayList;
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

		assertEquals(2, users.size());
		assertEquals(4, roles.size());

		// TODO extend assertions to actual model
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
		assertEquals("a2127d20a61e00bcdbb61569cd2b200c4f0f111c972bac3b1e54df3b2fcdc8be", fileHash);
	}
}
