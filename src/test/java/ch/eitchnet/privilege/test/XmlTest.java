/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the Privilege.
 *
 *  Privilege is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  Privilege is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Privilege.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package ch.eitchnet.privilege.test;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import junit.framework.Assert;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.handler.DefaultEncryptionHandler;
import ch.eitchnet.privilege.handler.XmlPersistenceHandler;
import ch.eitchnet.privilege.model.UserState;
import ch.eitchnet.privilege.model.internal.Privilege;
import ch.eitchnet.privilege.model.internal.PrivilegeContainerModel;
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
 * 
 */
public class XmlTest {

	/**
	 * 
	 */
	private static final String TARGET_TEST = "./target/test";
	private static final Logger logger = LoggerFactory.getLogger(XmlTest.class);

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@BeforeClass
	public static void init() throws Exception {

		File tmpDir = new File("target/test");
		if (tmpDir.exists())
			FileHelper.deleteFile(tmpDir, false);
		tmpDir.mkdirs();
	}

	@AfterClass
	public static void destroy() throws Exception {

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
		Assert.assertNotNull(containerModel.getParameterMap());
		Assert.assertNotNull(containerModel.getPolicies());
		Assert.assertNotNull(containerModel.getEncryptionHandlerClassName());
		Assert.assertNotNull(containerModel.getEncryptionHandlerParameterMap());
		Assert.assertNotNull(containerModel.getPersistenceHandlerClassName());
		Assert.assertNotNull(containerModel.getPersistenceHandlerParameterMap());

		Assert.assertEquals(1, containerModel.getParameterMap().size());
		Assert.assertEquals(1, containerModel.getPolicies().size());
		Assert.assertEquals(1, containerModel.getEncryptionHandlerParameterMap().size());
		Assert.assertEquals(2, containerModel.getPersistenceHandlerParameterMap().size());

		// TODO extend assertions to actual model
	}

	@Test
	public void canWriteConfig() {

		Map<String, String> parameterMap = new HashMap<String, String>();
		Map<String, String> encryptionHandlerParameterMap = new HashMap<String, String>();
		Map<String, String> persistenceHandlerParameterMap = new HashMap<String, String>();

		// TODO ask other questions...
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
		Assert.assertEquals("22d4ba39605d49c758184d9bd63beae5ccf8786f3dabbab45cd9f59c2afbcbd0", fileHash);
	}

	@Test
	public void canReadModel() {

		PrivilegeModelSaxReader xmlHandler = new PrivilegeModelSaxReader();
		File xmlFile = new File("config/PrivilegeModel.xml");
		XmlHelper.parseDocument(xmlFile, xmlHandler);

		List<User> users = xmlHandler.getUsers();
		Assert.assertNotNull(users);
		List<Role> roles = xmlHandler.getRoles();
		Assert.assertNotNull(roles);

		Assert.assertEquals(2, users.size());
		Assert.assertEquals(4, roles.size());

		// TODO extend assertions to actual model
	}

	@Test
	public void canWriteModel() {

		Map<String, String> propertyMap;
		Set<String> userRoles;
		Map<String, Privilege> privilegeMap;

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
		privilegeMap = new HashMap<String, Privilege>();
		privilegeMap.put("priv1", new Privilege("priv1", "DefaultPrivilege", true, null, null));
		roles.add(new Role("role1", privilegeMap));

		privilegeMap = new HashMap<String, Privilege>();
		Set<String> denyList = new HashSet<String>();
		denyList.add("myself");
		Set<String> allowList = new HashSet<String>();
		allowList.add("other");
		privilegeMap.put("priv2", new Privilege("priv2", "DefaultPrivilege", false, denyList, allowList));
		roles.add(new Role("role2", privilegeMap));

		File modelFile = new File("./target/test/PrivilegeModelTest.xml");
		PrivilegeModelDomWriter configSaxWriter = new PrivilegeModelDomWriter(users, roles, modelFile);
		configSaxWriter.write();

		String fileHash = StringHelper.getHexString(FileHelper.hashFileSha256(modelFile));
		Assert.assertEquals("8e1e82278162f21b1654c2e059570bbcb3cb63b053c1dd784bc8e225e8cfd04f", fileHash);
	}
}
