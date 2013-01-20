/*
 * Copyright (c) 2010 - 2012
 * 
 * This file is part of Privilege.
 *
 * Privilege is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Privilege is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Privilege.  If not, see <http://www.gnu.org/licenses/>.
 * 
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
import ch.eitchnet.privilege.helper.CertificateThreadLocal;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeRep;
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.RoleRep;
import ch.eitchnet.privilege.model.UserRep;
import ch.eitchnet.privilege.model.UserState;
import ch.eitchnet.privilege.test.model.TestRestrictable;
import ch.eitchnet.privilege.test.model.TestSystemUserAction;
import ch.eitchnet.privilege.test.model.TestSystemUserActionDeny;
import ch.eitchnet.privilege.xml.InitializationHelper;
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
			// copy configuration to tmp
			String pwd = System.getProperty("user.dir");

			File origPrivilegeModelFile = new File(pwd + "/config/PrivilegeModel.xml");
			File tmpPrivilegeModelFile = new File(pwd + "/target/test/PrivilegeModel.xml");
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
			PrivilegeTest.logger.error(e.getMessage(), e);

			throw new RuntimeException("Initialization failed: " + e.getLocalizedMessage(), e);
		}
	}

	@AfterClass
	public static void destroy() throws Exception {

		// delete temporary file
		String pwd = System.getProperty("user.dir");

		File tmpPrivilegeModelFile = new File(pwd + "/target/test/PrivilegeModel.xml");
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
			PrivilegeTest.privilegeHandler = InitializationHelper.initializeFromXml(privilegeConfigFile);

		} catch (Exception e) {
			PrivilegeTest.logger.error(e.getMessage(), e);

			throw new RuntimeException("Setup failed: " + e.getLocalizedMessage(), e);
		}
	}

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test
	public void testAuthenticationOk() throws Exception {

		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.ADMIN,
				copyBytes(PrivilegeTest.PASS_ADMIN));
		Assert.assertTrue("Certificate is null!", certificate != null);
		PrivilegeTest.privilegeHandler.invalidateSession(certificate);
	}

	private byte[] copyBytes(byte[] bytes) {
		byte[] copy = new byte[bytes.length];
		System.arraycopy(bytes, 0, copy, 0, bytes.length);
		return copy;
	}

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test(expected = AccessDeniedException.class)
	public void testFailAuthenticationNOk() throws Exception {

		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.ADMIN,
				copyBytes(PrivilegeTest.PASS_BAD));
		Assert.assertTrue("Certificate is null!", certificate != null);
		PrivilegeTest.privilegeHandler.invalidateSession(certificate);
	}

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test(expected = PrivilegeException.class)
	public void testFailAuthenticationPWNull() throws Exception {

		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.ADMIN, null);
		Assert.assertTrue("Certificate is null!", certificate != null);
		PrivilegeTest.privilegeHandler.invalidateSession(certificate);
	}

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test
	public void testAddUserBobAsAdmin() throws Exception {

		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.ADMIN,
				copyBytes(PrivilegeTest.PASS_ADMIN));

		// let's add a new user bob
		UserRep userRep = new UserRep("1", PrivilegeTest.BOB, "Bob", "Newman", UserState.NEW, new HashSet<String>(),
				null, new HashMap<String, String>());
		PrivilegeTest.privilegeHandler.addOrReplaceUser(certificate, userRep, null);
		PrivilegeTest.logger.info("Added user " + PrivilegeTest.BOB);

		// set bob's password
		PrivilegeTest.privilegeHandler.setUserPassword(certificate, PrivilegeTest.BOB,
				copyBytes(PrivilegeTest.PASS_BOB));
		PrivilegeTest.logger.info("Set Bob's password");
		privilegeHandler.persist(certificate);
		PrivilegeTest.privilegeHandler.invalidateSession(certificate);
	}

	/**
	 * Will fail because user bob is not yet enabled
	 * 
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test(expected = AccessDeniedException.class)
	public void testFailAuthAsBob() throws Exception {
		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.BOB,
				copyBytes(PrivilegeTest.PASS_BOB));
		PrivilegeTest.privilegeHandler.invalidateSession(certificate);
	}

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test
	public void testEnableUserBob() throws Exception {
		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.ADMIN,
				copyBytes(PrivilegeTest.PASS_ADMIN));
		PrivilegeTest.privilegeHandler.setUserState(certificate, PrivilegeTest.BOB, UserState.ENABLED);
		privilegeHandler.persist(certificate);
		PrivilegeTest.privilegeHandler.invalidateSession(certificate);
	}

	/**
	 * Will fail as user bob has no role
	 * 
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test(expected = PrivilegeException.class)
	public void testFailAuthUserBob() throws Exception {

		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.BOB,
				copyBytes(PrivilegeTest.PASS_BOB));
		Assert.assertTrue("Certificate is null!", certificate != null);
		PrivilegeTest.privilegeHandler.invalidateSession(certificate);
	}

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test
	public void testAddRole() throws Exception {
		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.ADMIN,
				copyBytes(PrivilegeTest.PASS_ADMIN));

		Map<String, PrivilegeRep> privilegeMap = new HashMap<String, PrivilegeRep>();
		RoleRep roleRep = new RoleRep(PrivilegeTest.ROLE_USER, privilegeMap);

		PrivilegeTest.privilegeHandler.addOrReplaceRole(certificate, roleRep);
		privilegeHandler.persist(certificate);
		PrivilegeTest.privilegeHandler.invalidateSession(certificate);
	}

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test
	public void testAddRoleToBob() throws Exception {
		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.ADMIN,
				copyBytes(PrivilegeTest.PASS_ADMIN));
		PrivilegeTest.privilegeHandler.addRoleToUser(certificate, PrivilegeTest.BOB, PrivilegeTest.ROLE_USER);
		privilegeHandler.persist(certificate);
		PrivilegeTest.privilegeHandler.invalidateSession(certificate);
	}

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test
	public void testAuthAsBob() throws Exception {
		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.BOB,
				copyBytes(PrivilegeTest.PASS_BOB));
		PrivilegeTest.privilegeHandler.invalidateSession(certificate);
	}

	/**
	 * Will fail because user bob does not have admin rights
	 * 
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test(expected = AccessDeniedException.class)
	public void testFailAddUserTedAsBob() throws Exception {

		// auth as Bog
		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.BOB,
				copyBytes(PrivilegeTest.PASS_BOB));
		Assert.assertTrue("Certificate is null!", certificate != null);

		// let's add a new user Ted
		UserRep userRep = new UserRep("1", PrivilegeTest.TED, "Ted", "And then Some", UserState.NEW,
				new HashSet<String>(), null, new HashMap<String, String>());
		PrivilegeTest.privilegeHandler.addOrReplaceUser(certificate, userRep, null);
		PrivilegeTest.logger.info("Added user " + PrivilegeTest.TED);
		PrivilegeTest.privilegeHandler.invalidateSession(certificate);
	}

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test
	public void testAddAdminRoleToBob() throws Exception {

		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.ADMIN,
				copyBytes(PrivilegeTest.PASS_ADMIN));
		PrivilegeTest.privilegeHandler.addRoleToUser(certificate, PrivilegeTest.BOB,
				PrivilegeHandler.PRIVILEGE_ADMIN_ROLE);
		PrivilegeTest.logger.info("Added " + PrivilegeHandler.PRIVILEGE_ADMIN_ROLE + " to " + PrivilegeTest.ADMIN);
		privilegeHandler.persist(certificate);
		PrivilegeTest.privilegeHandler.invalidateSession(certificate);
	}

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test
	public void testAddUserTedAsBob() throws Exception {

		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.BOB,
				copyBytes(PrivilegeTest.PASS_BOB));
		Assert.assertTrue("Certificate is null!", certificate != null);

		// let's add a new user ted
		HashSet<String> roles = new HashSet<String>();
		roles.add(PrivilegeTest.ROLE_USER);
		UserRep userRep = new UserRep("2", PrivilegeTest.TED, "Ted", "Newman", UserState.ENABLED, roles, null,
				new HashMap<String, String>());
		PrivilegeTest.privilegeHandler.addOrReplaceUser(certificate, userRep, null);
		PrivilegeTest.logger.info("Added user " + PrivilegeTest.TED);
		privilegeHandler.persist(certificate);
		PrivilegeTest.privilegeHandler.invalidateSession(certificate);
	}

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test
	public void testSetTedPwdAsBob() throws Exception {

		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.BOB,
				copyBytes(PrivilegeTest.PASS_BOB));
		Assert.assertTrue("Certificate is null!", certificate != null);

		// set ted's password to default
		PrivilegeTest.privilegeHandler.setUserPassword(certificate, PrivilegeTest.TED,
				copyBytes(PrivilegeTest.PASS_DEF));
		privilegeHandler.persist(certificate);
		PrivilegeTest.privilegeHandler.invalidateSession(certificate);
	}

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test
	public void testTedChangesOwnPwd() throws Exception {
		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.TED,
				copyBytes(PrivilegeTest.PASS_DEF));
		PrivilegeTest.privilegeHandler.setUserPassword(certificate, PrivilegeTest.TED,
				copyBytes(PrivilegeTest.PASS_TED));
		PrivilegeTest.privilegeHandler.invalidateSession(certificate);
	}

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test
	public void testAuthAsTed() throws Exception {
		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.TED,
				copyBytes(PrivilegeTest.PASS_TED));
		PrivilegeTest.privilegeHandler.invalidateSession(certificate);
	}

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test
	public void testPerformRestrictableAsAdmin() throws Exception {

		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.ADMIN,
				copyBytes(PrivilegeTest.PASS_ADMIN));
		Assert.assertTrue("Certificate is null!", certificate != null);

		// see if eitch can perform restrictable
		Restrictable restrictable = new TestRestrictable();
		PrivilegeTest.privilegeHandler.actionAllowed(certificate, restrictable);
		PrivilegeTest.privilegeHandler.invalidateSession(certificate);
	}

	/**
	 * Tests if the user bob, who does not have AppUser role can perform restrictable
	 * 
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test(expected = AccessDeniedException.class)
	public void testFailPerformRestrictableAsBob() throws Exception {
		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.BOB,
				copyBytes(PrivilegeTest.PASS_BOB));
		Assert.assertTrue("Certificate is null!", certificate != null);

		// see if bob can perform restrictable
		Restrictable restrictable = new TestRestrictable();
		try {
			PrivilegeTest.privilegeHandler.actionAllowed(certificate, restrictable);
		} finally {
			PrivilegeTest.privilegeHandler.invalidateSession(certificate);
		}
	}

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test
	public void testAddAppRoleToBob() throws Exception {

		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.ADMIN,
				copyBytes(PrivilegeTest.PASS_ADMIN));
		PrivilegeTest.privilegeHandler.addRoleToUser(certificate, PrivilegeTest.BOB, PrivilegeTest.ROLE_APP_USER);
		PrivilegeTest.logger.info("Added " + PrivilegeTest.ROLE_APP_USER + " to " + PrivilegeTest.BOB);
		privilegeHandler.persist(certificate);
		PrivilegeTest.privilegeHandler.invalidateSession(certificate);
	}

	/**
	 * Tests if the user bob, who now has AppUser role can perform restrictable
	 * 
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test
	public void testPerformRestrictableAsBob() throws Exception {
		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.BOB,
				copyBytes(PrivilegeTest.PASS_BOB));
		Assert.assertTrue("Certificate is null!", certificate != null);

		// see if bob can perform restrictable
		Restrictable restrictable = new TestRestrictable();
		try {
			PrivilegeTest.privilegeHandler.actionAllowed(certificate, restrictable);
		} finally {
			PrivilegeTest.privilegeHandler.invalidateSession(certificate);
		}
	}

	/**
	 * Tests if an action can be performed as a system user
	 * 
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test
	public void testPerformSystemRestrictable() throws Exception {

		// create the action to be performed as a system user
		TestSystemUserAction action = new TestSystemUserAction(PrivilegeTest.privilegeHandler);

		// and then perform the action
		PrivilegeTest.privilegeHandler.runAsSystem(PrivilegeTest.SYSTEM_USER_ADMIN, action);
	}

	/**
	 * Checks that the system user can not perform a valid action, but illegal privilege
	 * 
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test(expected = PrivilegeException.class)
	public void testPerformSystemRestrictableFailPrivilege() throws Exception {

		// create the action to be performed as a system user
		TestSystemUserActionDeny action = new TestSystemUserActionDeny(PrivilegeTest.privilegeHandler);

		// and then perform the action
		PrivilegeTest.privilegeHandler.runAsSystem(PrivilegeTest.SYSTEM_USER_ADMIN, action);
	}

	/**
	 * System user may not login
	 * 
	 * @throws Exception
	 *             if something goes wrong
	 */
	@Test(expected = AccessDeniedException.class)
	public void testLoginSystemUser() throws Exception {

		PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.SYSTEM_USER_ADMIN,
				PrivilegeTest.SYSTEM_USER_ADMIN.getBytes());
	}

	@Test
	public void testCertificateThreadLocal() {

		Certificate certificate = PrivilegeTest.privilegeHandler.authenticate(PrivilegeTest.ADMIN,
				copyBytes(PrivilegeTest.PASS_ADMIN));
		Assert.assertTrue("Certificate is null!", certificate != null);

		// set certificate into thread local
		CertificateThreadLocal.getInstance().set(certificate);

		// see if bob can perform restrictable by returning certificate from CertificateThreadLocal
		Restrictable restrictable = new TestRestrictable();
		try {
			PrivilegeTest.privilegeHandler.actionAllowed(CertificateThreadLocal.getInstance().get(), restrictable);
		} finally {
			PrivilegeTest.privilegeHandler.invalidateSession(certificate);
		}
	}
}
