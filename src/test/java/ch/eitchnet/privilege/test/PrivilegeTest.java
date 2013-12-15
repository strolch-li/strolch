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

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import junit.framework.Assert;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.base.AccessDeniedException;
import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.helper.PrivilegeInitializationHelper;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.privilege.model.PrivilegeRep;
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.RoleRep;
import ch.eitchnet.privilege.model.UserRep;
import ch.eitchnet.privilege.model.UserState;
import ch.eitchnet.privilege.test.model.TestRestrictable;
import ch.eitchnet.privilege.test.model.TestSystemUserAction;
import ch.eitchnet.privilege.test.model.TestSystemUserActionDeny;
import ch.eitchnet.utils.helper.ArraysHelper;
import ch.eitchnet.utils.helper.FileHelper;

/**
 * JUnit for performing Privilege tests. This JUnit is by no means complete, but checks the bare minimum.br />
 * 
 * TODO add more tests, especially with deny and allow lists
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeTest {

	private static final String ADMIN = "admin";
	private static final byte[] PASS_ADMIN = "admin".getBytes();
	private static final String BOB = "bob";
	private static final String TED = "ted";
	private static final String SYSTEM_USER_ADMIN = "system_admin";
	private static final byte[] PASS_BOB = "admin1".getBytes();
	private static final String ROLE_APP_USER = "AppUser";
	private static final String ROLE_TEMP = "temp";
	private static final String ROLE_USER = "user";
	private static final byte[] PASS_DEF = "def".getBytes();
	private static final byte[] PASS_BAD = "123".getBytes();
	private static final byte[] PASS_TED = "12345".getBytes();

	private static final Logger logger = LoggerFactory.getLogger(PrivilegeTest.class);

	private static PrivilegeHandler privilegeHandler;

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@BeforeClass
	public static void init() throws Exception {
		try {
			destroy();
			
			// copy configuration to tmp
			String pwd = System.getProperty("user.dir");

			File origPrivilegeModelFile = new File(pwd + "/config/PrivilegeModel.xml");
			File tmpPrivilegeModelFile = new File(pwd + "/target/testPrivilege/PrivilegeModel.xml");
			if (tmpPrivilegeModelFile.exists() && !tmpPrivilegeModelFile.delete()) {
				throw new RuntimeException("Tmp configuration still exists and can not be deleted at "
						+ tmpPrivilegeModelFile.getAbsolutePath());
			}

			File parentFile = tmpPrivilegeModelFile.getParentFile();
			if (!parentFile.exists()) {
				if (!parentFile.mkdirs())
					throw new RuntimeException("Could not create parent for tmp " + tmpPrivilegeModelFile);
			}

			if (!FileHelper.copy(origPrivilegeModelFile, tmpPrivilegeModelFile, true))
				throw new RuntimeException("Failed to copy " + origPrivilegeModelFile + " to " + tmpPrivilegeModelFile);

		} catch (Exception e) {
			logger.error(e.getMessage(), e);

			throw new RuntimeException("Initialization failed: " + e.getLocalizedMessage(), e);
		}
	}

	@AfterClass
	public static void destroy() throws Exception {

		// delete temporary file
		String pwd = System.getProperty("user.dir");

		File tmpPrivilegeModelFile = new File(pwd + "/target/testPrivilege/PrivilegeModel.xml");
		if (tmpPrivilegeModelFile.exists() && !tmpPrivilegeModelFile.delete()) {
			throw new RuntimeException("Tmp configuration still exists and can not be deleted at "
					+ tmpPrivilegeModelFile.getAbsolutePath());
		}

		// and temporary parent
		File parentFile = tmpPrivilegeModelFile.getParentFile();
		if (parentFile.exists() && !parentFile.delete()) {
			throw new RuntimeException("Could not remove temporary parent for tmp " + tmpPrivilegeModelFile);
		}
	}

	@Before
	public void setup() throws Exception {
		try {

			String pwd = System.getProperty("user.dir");

			File privilegeConfigFile = new File(pwd + "/config/Privilege.xml");

			// initialize privilege
			privilegeHandler = PrivilegeInitializationHelper.initializeFromXml(privilegeConfigFile);

		} catch (Exception e) {
			logger.error(e.getMessage(), e);

			throw new RuntimeException("Setup failed: " + e.getLocalizedMessage(), e);
		}
	}

	private void login(String username, byte[] password) {
		Certificate certificate = privilegeHandler.authenticate(username, password);
		Assert.assertTrue("Certificate is null!", certificate != null);
		PrivilegeContext privilegeContext = privilegeHandler.getPrivilegeContext(certificate);
		PrivilegeContext.set(privilegeContext);
	}

	private void logout() {
		try {
			PrivilegeContext privilegeContext = PrivilegeContext.get();
			privilegeHandler.invalidateSession(privilegeContext.getCertificate());
		} catch (PrivilegeException e) {
			String msg = "There is no PrivilegeContext currently bound to the ThreadLocal!";
			if (!e.getMessage().equals(msg))
				throw e;
		} finally {
			PrivilegeContext.set(null);
		}
	}

	@Test
	public void testAuthenticationOk() throws Exception {
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));
		} finally {
			logout();
		}
	}

	@Test(expected = AccessDeniedException.class)
	public void testFailAuthenticationNOk() throws Exception {
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_BAD));
		} finally {
			logout();
		}
	}

	@Test(expected = PrivilegeException.class)
	public void testFailAuthenticationPWNull() throws Exception {
		try {
			login(ADMIN, null);
		} finally {
			logout();
		}
	}

	@Test
	public void testAddRoleTemp() throws Exception {
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));

			Map<String, PrivilegeRep> privilegeMap = new HashMap<String, PrivilegeRep>();
			RoleRep roleRep = new RoleRep(ROLE_TEMP, privilegeMap);

			Certificate certificate = PrivilegeContext.get().getCertificate();
			privilegeHandler.addOrReplaceRole(certificate, roleRep);
			privilegeHandler.persist(certificate);
		} finally {
			logout();
		}
	}

	@Test
	public void testPerformRestrictableAsAdmin() throws Exception {
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));

			// see if admin can perform restrictable
			Restrictable restrictable = new TestRestrictable();
			PrivilegeContext.get().validateAction(restrictable);

		} finally {
			logout();
		}
	}

	/**
	 * Tests if an action can be performed as a system user
	 */
	@Test
	public void testPerformSystemRestrictable() throws Exception {

		// create the action to be performed as a system user and then perform the action
		TestSystemUserAction action = new TestSystemUserAction();
		privilegeHandler.runAsSystem(SYSTEM_USER_ADMIN, action);
	}

	/**
	 * Checks that the system user can not perform a valid action, but illegal privilege
	 */
	@Test(expected = PrivilegeException.class)
	public void testPerformSystemRestrictableFailPrivilege() throws Exception {
		try {
			// create the action to be performed as a system user
			TestSystemUserActionDeny action = new TestSystemUserActionDeny();

			// and then perform the action
			privilegeHandler.runAsSystem(SYSTEM_USER_ADMIN, action);
		} finally {
			logout();
		}
	}

	/**
	 * System user may not login
	 */
	@Test(expected = AccessDeniedException.class)
	public void testLoginSystemUser() throws Exception {
		try {
			login(SYSTEM_USER_ADMIN, SYSTEM_USER_ADMIN.getBytes());
		} finally {
			logout();
		}
	}

	@Test
	public void testPrivilegeContext() {
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));
			Restrictable restrictable = new TestRestrictable();
			PrivilegeContext.get().validateAction(restrictable);
		} finally {
			logout();
		}
	}

	/**
	 * This test performs multiple tests which are dependent on each other as the following is done:
	 * <ul>
	 * <li>add user bob</li>
	 * <li>fail to auth as bob as user is not enabled</li>
	 * <li>enable bob</li>
	 * <li>fail to auth as bot as bob has no role</li>
	 * <li>add role user to bob</li>
	 * <li>auth as bob</li>
	 * <li>fail to add user ted as bob as bob is not admin</li>
	 * <li>add admin role to bob</li>
	 * <li>add user ted as bob</li>
	 * <li>fail to auth as ted as ted has no password</li>
	 * <li>set ted's password as bob</li>
	 * <li>ted changes own password</li>
	 * <li>auth as ted</li>
	 * <li>fail to perform restrictable as bob as no app role</li>
	 * <li>add app role to bob</li>
	 * <li>perform restrictable as bob</li>
	 * </ul>
	 * 
	 */
	@Test
	public void testUserStory() throws Exception {

		addBobAsAdmin();
		failAuthAsBobNotEnabled();
		enableBob();
		failAuthAsBobNoRole();
		addRoleUser();
		addRoleUserToBob();
		authAsBob();
		failAddTedAsBobNotAdmin();
		addRoleAdminToBob();
		addTedAsBob();
		failAuthAsTedNoPass();
		setPassForTedAsBob();
		tedChangesOwnPass();
		authAsTed();
		failPerformRestrictableAsBobNoRoleApp();
		addRoleAppToBob();
		performRestrictableAsBob();
	}

	private void performRestrictableAsBob() {
		try {
			// testPerformRestrictableAsBob
			// Tests if the user bob, who now has AppUser role can perform restrictable
			login(BOB, ArraysHelper.copyOf(PASS_BOB));
			// see if bob can perform restrictable
			Restrictable restrictable = new TestRestrictable();
			PrivilegeContext.get().validateAction(restrictable);
		} finally {
			logout();
		}
	}

	private void addRoleAppToBob() {
		try {
			// testAddAppRoleToBob
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));
			Certificate certificate = PrivilegeContext.get().getCertificate();
			privilegeHandler.addRoleToUser(certificate, BOB, ROLE_APP_USER);
			logger.info("Added " + ROLE_APP_USER + " to " + BOB);
			privilegeHandler.persist(certificate);
		} finally {
			logout();
		}
	}

	private void failPerformRestrictableAsBobNoRoleApp() {
		try {
			// testFailPerformRestrictableAsBob
			// Tests if the user bob, who does not have AppUser role can perform restrictable
			// this will fail as bob does not have role app
			login(BOB, ArraysHelper.copyOf(PASS_BOB));
			// see if bob can perform restrictable
			Restrictable restrictable = new TestRestrictable();
			PrivilegeContext.get().validateAction(restrictable);
			Assert.fail("Should fail as bob does not have role app");
		} catch (AccessDeniedException e) {
			String msg = "User bob does not have Privilege ch.eitchnet.privilege.test.model.TestRestrictable needed for Restrictable ch.eitchnet.privilege.test.model.TestRestrictable";
			Assert.assertEquals(msg, e.getLocalizedMessage());
		} finally {
			logout();
		}
	}

	private void authAsTed() {
		try {
			// testAuthAsTed
			login(TED, ArraysHelper.copyOf(PASS_TED));
		} finally {
			logout();
		}
	}

	private void tedChangesOwnPass() {
		try {
			// testTedChangesOwnPwd
			login(TED, ArraysHelper.copyOf(PASS_DEF));
			Certificate certificate = PrivilegeContext.get().getCertificate();
			privilegeHandler.setUserPassword(certificate, TED, ArraysHelper.copyOf(PASS_TED));
		} finally {
			logout();
		}
	}

	private void setPassForTedAsBob() {
		try {
			// testSetTedPwdAsBob
			login(BOB, ArraysHelper.copyOf(PASS_BOB));
			// set ted's password to default
			Certificate certificate = PrivilegeContext.get().getCertificate();
			privilegeHandler.setUserPassword(certificate, TED, ArraysHelper.copyOf(PASS_DEF));
			privilegeHandler.persist(certificate);
		} finally {
			logout();
		}
	}

	private void failAuthAsTedNoPass() {
		try {
			// testFailAuthAsTedNoPass
			// Will fail because user ted has no password
			login(TED, ArraysHelper.copyOf(PASS_TED));
			org.junit.Assert.fail("User Ted may not authenticate because the user has no password!");
		} catch (PrivilegeException e) {
			String msg = "User ted has no password and may not login!";
			Assert.assertEquals(msg, e.getMessage());
		} finally {
			logout();
		}
	}

	private void addTedAsBob() {
		try {
			UserRep userRep;
			// testAddUserTedAsBob
			login(BOB, ArraysHelper.copyOf(PASS_BOB));
			// let's add a new user ted
			HashSet<String> roles = new HashSet<String>();
			roles.add(ROLE_USER);
			userRep = new UserRep("2", TED, "Ted", "Newman", UserState.ENABLED, roles, null,
					new HashMap<String, String>());
			Certificate certificate = PrivilegeContext.get().getCertificate();
			privilegeHandler.addOrReplaceUser(certificate, userRep, null);
			logger.info("Added user " + TED);
			privilegeHandler.persist(certificate);
		} finally {
			logout();
		}
	}

	private void addRoleAdminToBob() {
		try {
			// testAddAdminRoleToBob
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));
			Certificate certificate = PrivilegeContext.get().getCertificate();
			privilegeHandler.addRoleToUser(certificate, BOB, PrivilegeHandler.PRIVILEGE_ADMIN_ROLE);
			logger.info("Added " + PrivilegeHandler.PRIVILEGE_ADMIN_ROLE + " to " + ADMIN);
			privilegeHandler.persist(certificate);
		} finally {
			logout();
		}
	}

	private void failAddTedAsBobNotAdmin() {
		Certificate certificate = null;
		try {
			UserRep userRep;
			// testFailAddUserTedAsBob
			// Will fail because user bob does not have admin rights		
			// auth as Bob
			login(BOB, ArraysHelper.copyOf(PASS_BOB));
			// let's add a new user Ted
			userRep = new UserRep("1", TED, "Ted", "And then Some", UserState.NEW, new HashSet<String>(), null,
					new HashMap<String, String>());
			certificate = PrivilegeContext.get().getCertificate();
			privilegeHandler.addOrReplaceUser(certificate, userRep, null);
			Assert.fail("User bob may not add a user as bob does not have admin rights!");
		} catch (PrivilegeException e) {
			String msg = "User does not have PrivilegeAdmin role! Certificate: " + certificate;
			Assert.assertEquals(msg, e.getMessage());
		} finally {
			logout();
		}
	}

	private void authAsBob() {
		try {
			// testAuthAsBob
			login(BOB, ArraysHelper.copyOf(PASS_BOB));
		} finally {
			logout();
		}
	}

	private void addRoleUserToBob() {
		try {
			// testAddRoleUserToBob
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));
			Certificate certificate = PrivilegeContext.get().getCertificate();
			privilegeHandler.addRoleToUser(certificate, BOB, ROLE_USER);
			privilegeHandler.persist(certificate);
			logout();
		} finally {
			logout();
		}
	}

	private void addRoleUser() {
		try {
			// add role user
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));
			Map<String, PrivilegeRep> privilegeMap = new HashMap<String, PrivilegeRep>();
			RoleRep roleRep = new RoleRep(ROLE_USER, privilegeMap);
			Certificate certificate = PrivilegeContext.get().getCertificate();
			privilegeHandler.addOrReplaceRole(certificate, roleRep);
			privilegeHandler.persist(certificate);
		} finally {
			logout();
		}
	}

	private void failAuthAsBobNoRole() {
		try {
			// testFailAuthUserBob
			// Will fail as user bob has no role
			privilegeHandler.authenticate(BOB, ArraysHelper.copyOf(PASS_BOB));
			org.junit.Assert.fail("User Bob may not authenticate because the user has no role");
		} catch (PrivilegeException e) {
			String msg = "User bob does not have any roles defined!";
			Assert.assertEquals(msg, e.getMessage());
		} finally {
			logout();
		}
	}

	private void enableBob() {
		try {
			// testEnableUserBob
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));
			Certificate certificate = PrivilegeContext.get().getCertificate();
			privilegeHandler.setUserState(certificate, BOB, UserState.ENABLED);
			privilegeHandler.persist(certificate);
		} finally {
			logout();
		}
	}

	private void failAuthAsBobNotEnabled() {
		try {
			// testFailAuthAsBob
			// Will fail because user bob is not yet enabled
			privilegeHandler.authenticate(BOB, ArraysHelper.copyOf(PASS_BOB));
			org.junit.Assert.fail("User Bob may not authenticate because the user is not yet enabled!");
		} catch (PrivilegeException e) {
			String msg = "User bob does not have state ENABLED and can not login!";
			Assert.assertEquals(msg, e.getMessage());
		} finally {
			logout();
		}
	}

	private void addBobAsAdmin() {
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));

			// let's add a new user bob
			UserRep userRep = new UserRep("1", BOB, "Bob", "Newman", UserState.NEW, new HashSet<String>(), null,
					new HashMap<String, String>());
			Certificate certificate = PrivilegeContext.get().getCertificate();
			privilegeHandler.addOrReplaceUser(certificate, userRep, null);
			logger.info("Added user " + BOB);

			// set bob's password
			privilegeHandler.setUserPassword(certificate, BOB, ArraysHelper.copyOf(PASS_BOB));
			logger.info("Set Bob's password");
			privilegeHandler.persist(certificate);
		} finally {
			logout();
		}
	}
}
