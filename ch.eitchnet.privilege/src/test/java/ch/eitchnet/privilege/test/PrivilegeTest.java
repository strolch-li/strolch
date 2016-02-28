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
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.fail;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.base.AccessDeniedException;
import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.i18n.PrivilegeMessages;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeRep;
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.RoleRep;
import ch.eitchnet.privilege.model.UserRep;
import ch.eitchnet.privilege.model.UserState;
import ch.eitchnet.privilege.test.model.TestRestrictable;
import ch.eitchnet.privilege.test.model.TestSystemUserAction;
import ch.eitchnet.privilege.test.model.TestSystemUserActionDeny;
import ch.eitchnet.utils.helper.ArraysHelper;

/**
 * JUnit for performing Privilege tests. This JUnit is by no means complete, but checks the bare minimum.br />
 * 
 * TODO add more tests, especially with deny and allow lists
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class PrivilegeTest extends AbstractPrivilegeTest {

	private static final String ROLE_PRIVILEGE_ADMIN = "PrivilegeAdmin";
	private static final String PRIVILEGE_USER_ACCESS = "UserAccessPrivilege";
	private static final String ADMIN = "admin";
	private static final byte[] PASS_ADMIN = "admin".getBytes();
	private static final String BOB = "bob";
	private static final String TED = "ted";
	private static final String SYSTEM_USER_ADMIN = "system_admin";
	private static final String SYSTEM_USER_ADMIN2 = "system_admin2";
	private static final byte[] PASS_BOB = "admin1".getBytes();
	private static final String ROLE_APP_USER = "AppUser";
	private static final String ROLE_MY = "MyRole";
	private static final String ROLE_MY2 = "MyRole2";
	private static final String ROLE_CHANGE_PW = "changePw";
	private static final String ROLE_TEMP = "temp";
	private static final String ROLE_USER = "user";
	private static final byte[] PASS_DEF = "def".getBytes();
	private static final byte[] PASS_BAD = "123".getBytes();
	private static final byte[] PASS_TED = "12345".getBytes();

	private static final Logger logger = LoggerFactory.getLogger(PrivilegeTest.class);

	@Rule
	public ExpectedException exception = ExpectedException.none();

	@BeforeClass
	public static void init() throws Exception {
		removeConfigs(PrivilegeTest.class.getSimpleName());
		prepareConfigs(PrivilegeTest.class.getSimpleName(), "PrivilegeConfig.xml", "PrivilegeUsers.xml",
				"PrivilegeRoles.xml");
	}

	@AfterClass
	public static void destroy() throws Exception {
		removeConfigs(PrivilegeTest.class.getSimpleName());
	}

	@Before
	public void setup() throws Exception {
		initialize(PrivilegeTest.class.getSimpleName(), "PrivilegeConfig.xml");
	}

	@Test
	public void testAuthenticationOk() throws Exception {
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));
		} finally {
			logout();
		}
	}

	public void testFailAuthenticationNOk() throws Exception {
		this.exception.expect(AccessDeniedException.class);
		this.exception.expectMessage("blabla");
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_BAD));
		} finally {
			logout();
		}
	}

	public void testFailAuthenticationPWNull() throws Exception {
		this.exception.expect(PrivilegeException.class);
		this.exception.expectMessage("blabla");
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

			RoleRep roleRep = new RoleRep(ROLE_TEMP, new ArrayList<>());

			Certificate certificate = this.ctx.getCertificate();
			privilegeHandler.addRole(certificate, roleRep);
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
			this.ctx.validateAction(restrictable);

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
	@Test
	public void testPerformSystemRestrictableFailPrivilege() throws Exception {
		this.exception.expect(PrivilegeException.class);
		this.exception.expectMessage(
				"User system_admin does not have the privilege ch.eitchnet.privilege.handler.SystemUserAction");
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
	 * Checks that the system user can not perform a valid action, but illegal privilege
	 */
	@Test
	public void testPerformSystemRestrictableFailNoAdditionalPrivilege() throws Exception {
		this.exception.expect(PrivilegeException.class);
		this.exception.expectMessage(
				"User system_admin2 does not have the privilege ch.eitchnet.privilege.handler.SystemUserAction needed for Restrictable ch.eitchnet.privilege.test.model.TestSystemUserActionDeny");
		try {
			// create the action to be performed as a system user
			TestSystemUserActionDeny action = new TestSystemUserActionDeny();

			// and then perform the action
			privilegeHandler.runAsSystem(SYSTEM_USER_ADMIN2, action);
		} finally {
			logout();
		}
	}

	/**
	 * System user may not login
	 */
	@Test
	public void testLoginSystemUser() throws Exception {
		this.exception.expect(AccessDeniedException.class);
		this.exception.expectMessage("User system_admin is a system user and may not login!");
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
			this.ctx.validateAction(restrictable);
		} finally {
			logout();
		}
	}

	@Test
	public void shouldUpdateAdmin() {
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));

			Certificate certificate = this.ctx.getCertificate();

			// validate name is not yet set
			UserRep user = privilegeHandler.getUser(certificate, ADMIN);
			assertNotEquals("The", user.getFirstname());
			assertNotEquals("Admin", user.getLastname());

			// let's add a new user bob
			UserRep userRep = new UserRep(null, ADMIN, "The", "Admin", null, null, null, null);
			privilegeHandler.updateUser(certificate, userRep);

			user = privilegeHandler.getUser(certificate, ADMIN);
			assertEquals("The", user.getFirstname());
			assertEquals("Admin", user.getLastname());

		} finally {
			logout();
		}
	}

	@Test
	public void shouldFailUpdateInexistantUser() {
		exception.expect(PrivilegeException.class);
		exception.expectMessage("User bob does not exist");
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));

			Certificate certificate = this.ctx.getCertificate();

			// let's add a new user bob
			UserRep userRep = new UserRep(null, BOB, null, null, null, null, null, null);
			privilegeHandler.updateUser(certificate, userRep);
		} finally {
			logout();
		}
	}

	@Test
	public void shouldFailUpdateAdminNoChanges() {
		exception.expect(PrivilegeException.class);
		exception.expectMessage("All updateable fields are empty for update of user admin");
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));

			Certificate certificate = this.ctx.getCertificate();

			// let's add a new user bob
			UserRep userRep = new UserRep(null, ADMIN, null, null, null, null, null, null);
			privilegeHandler.updateUser(certificate, userRep);
		} finally {
			logout();
		}
	}

	@Test
	public void shouldQueryUsers() {
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));

			Certificate certificate = this.ctx.getCertificate();

			UserRep selectorRep = new UserRep(null, ADMIN, null, null, null, null, null, null);
			List<UserRep> users = privilegeHandler.queryUsers(certificate, selectorRep);
			assertEquals(1, users.size());
			assertEquals(ADMIN, users.get(0).getUsername());

		} finally {
			logout();
		}
	}

	@Test
	public void shouldQueryUsersByRoles() {
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));

			Certificate certificate = this.ctx.getCertificate();

			UserRep selectorRep = new UserRep(null, null, null, null, null,
					new HashSet<>(Arrays.asList("PrivilegeAdmin")), null, null);
			List<UserRep> users = privilegeHandler.queryUsers(certificate, selectorRep);
			assertEquals(1, users.size());
			assertEquals(ADMIN, users.get(0).getUsername());

		} finally {
			logout();
		}
	}

	@Test
	public void shouldQueryUsersByRoles2() {
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));

			Certificate certificate = this.ctx.getCertificate();

			UserRep selectorRep = new UserRep(null, null, null, null, null, new HashSet<>(Arrays.asList(ROLE_TEMP)),
					null, null);
			List<UserRep> users = privilegeHandler.queryUsers(certificate, selectorRep);
			assertEquals(0, users.size());

		} finally {
			logout();
		}
	}

	@Test
	public void shouldDetectPrivilegeConflict1() {
		exception.expect(PrivilegeException.class);
		exception.expectMessage("User admin has conflicts for privilege ");
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));
			Certificate certificate = this.ctx.getCertificate();
			PrivilegeRep privilegeRep = new PrivilegeRep(PrivilegeHandler.PRIVILEGE_ACTION, "DefaultPrivilege", true,
					Collections.emptySet(), Collections.emptySet());
			privilegeHandler.addOrReplacePrivilegeOnRole(certificate, ROLE_APP_USER, privilegeRep);
		} finally {
			logout();
		}
	}

	@Test
	public void shouldDetectPrivilegeConflict2() {
		exception.expect(PrivilegeException.class);
		exception.expectMessage("User admin has conflicts for privilege ");
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));
			Certificate certificate = this.ctx.getCertificate();
			privilegeHandler.addRoleToUser(certificate, ADMIN, ROLE_MY);
			privilegeHandler.addRoleToUser(certificate, ADMIN, ROLE_MY2);
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
	 */
	@Test
	public void testUserStory() throws Exception {

		addBobAsAdmin();
		failAuthAsBobNotEnabled();
		enableBob();
		addRoleUser();
		addRoleUserToBob();
		authAsBob();
		failAddTedAsBobNotAdmin();
		addRoleAdminToBob();
		addTedAsBob();
		failAuthAsTedNoPass();
		setPassForTedAsBob();
		failSetSystemStateTed();
		failTedChangesPwAndLocale();
		addChangePwRoleToTed();
		tedChangesOwnPassAndLocale();
		failTedChangesOtherPwStateAndLocale();
		authAsTed();
		failPerformRestrictableAsBobNoRoleApp();
		addRoleAppToBob();
		performRestrictableAsBob();
	}

	private void failSetSystemStateTed() {
		try {
			// testEnableUserBob
			login(BOB, ArraysHelper.copyOf(PASS_BOB));
			Certificate certificate = this.ctx.getCertificate();

			try {
				privilegeHandler.setUserState(certificate, TED, UserState.SYSTEM);
				fail("Should not be able to set user state to SYSTEM");
			} catch (AccessDeniedException e) {
				// ok
			}

		} finally {
			logout();
		}
	}

	private void failTedChangesOtherPwStateAndLocale() {
		try {
			// testTedChangesOwnPwd
			login(TED, ArraysHelper.copyOf(PASS_TED));
			Certificate certificate = this.ctx.getCertificate();

			try {
				privilegeHandler.setUserPassword(certificate, BOB, ArraysHelper.copyOf(PASS_TED));
				fail("Should not be able to set password of other user, as missing privilege");
			} catch (AccessDeniedException e) {
				// ok
			}

			try {
				privilegeHandler.setUserLocale(certificate, BOB, Locale.FRENCH);
				fail("Should not be able to set locale of other user, as missing privilege");
			} catch (AccessDeniedException e) {
				// ok
			}

			try {
				privilegeHandler.setUserState(certificate, BOB, UserState.DISABLED);
				fail("Should not be able to set state of other user, as missing privilege");
			} catch (AccessDeniedException e) {
				// ok
			}

		} finally {
			logout();
		}
	}

	private void failTedChangesPwAndLocale() {
		try {
			// testTedChangesOwnPwd
			login(TED, ArraysHelper.copyOf(PASS_DEF));
			Certificate certificate = this.ctx.getCertificate();

			try {
				privilegeHandler.setUserPassword(certificate, TED, ArraysHelper.copyOf(PASS_TED));
				fail("Should not be able to set password, as missing privilege");
			} catch (AccessDeniedException e) {
				// ok
			}

			try {
				privilegeHandler.setUserLocale(certificate, TED, Locale.FRENCH);
				fail("Should not be able to set locale, as missing privilege");
			} catch (AccessDeniedException e) {
				// ok
			}

			try {
				privilegeHandler.setUserState(certificate, TED, UserState.ENABLED);
				fail("Should not be able to set state, as missing privilege");
			} catch (AccessDeniedException e) {
				// ok
			}

		} finally {
			logout();
		}
	}

	private void addChangePwRoleToTed() {
		try {
			// add role user
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));

			PrivilegeRep passwordRep = new PrivilegeRep(PrivilegeHandler.PRIVILEGE_SET_USER_PASSWORD,
					PRIVILEGE_USER_ACCESS, false, Collections.emptySet(), Collections.emptySet());
			PrivilegeRep localeRep = new PrivilegeRep(PrivilegeHandler.PRIVILEGE_SET_USER_LOCALE, PRIVILEGE_USER_ACCESS,
					false, Collections.emptySet(), Collections.emptySet());

			RoleRep roleRep = new RoleRep(ROLE_CHANGE_PW, Arrays.asList(passwordRep, localeRep));

			Certificate certificate = this.ctx.getCertificate();
			privilegeHandler.addRole(certificate, roleRep);
			privilegeHandler.addRoleToUser(certificate, TED, ROLE_CHANGE_PW);
			logger.info("Added " + ROLE_CHANGE_PW + " to " + TED);
			privilegeHandler.persist(certificate);
		} finally {
			logout();
		}
	}

	private void performRestrictableAsBob() {
		try {
			// testPerformRestrictableAsBob
			// Tests if the user bob, who now has AppUser role can perform restrictable
			login(BOB, ArraysHelper.copyOf(PASS_BOB));
			// see if bob can perform restrictable
			Restrictable restrictable = new TestRestrictable();
			this.ctx.validateAction(restrictable);
		} finally {
			logout();
		}
	}

	private void addRoleAppToBob() {
		try {
			// testAddAppRoleToBob
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));
			Certificate certificate = this.ctx.getCertificate();
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
			this.ctx.validateAction(restrictable);
			fail("Should fail as bob does not have role app");
		} catch (AccessDeniedException e) {
			String msg = "User bob does not have the privilege ch.eitchnet.privilege.test.model.TestRestrictable needed for Restrictable ch.eitchnet.privilege.test.model.TestRestrictable";
			assertEquals(msg, e.getLocalizedMessage());
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

	private void tedChangesOwnPassAndLocale() {
		try {
			// testTedChangesOwnPwd
			login(TED, ArraysHelper.copyOf(PASS_DEF));
			Certificate certificate = this.ctx.getCertificate();
			privilegeHandler.setUserPassword(certificate, TED, ArraysHelper.copyOf(PASS_TED));
			privilegeHandler.setUserLocale(certificate, TED, Locale.FRENCH);
		} finally {
			logout();
		}
	}

	private void setPassForTedAsBob() {
		try {
			// testSetTedPwdAsBob
			login(BOB, ArraysHelper.copyOf(PASS_BOB));
			// set ted's password to default
			Certificate certificate = this.ctx.getCertificate();
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
			fail("User Ted may not authenticate because the user has no password!");
		} catch (PrivilegeException e) {
			String msg = "User ted has no password and may not login!";
			assertEquals(msg, e.getMessage());
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
			HashSet<String> roles = new HashSet<>();
			roles.add(ROLE_USER);
			userRep = new UserRep(null, TED, "Ted", "Newman", UserState.ENABLED, roles, null,
					new HashMap<String, String>());
			Certificate certificate = this.ctx.getCertificate();
			privilegeHandler.addUser(certificate, userRep, null);
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
			Certificate certificate = this.ctx.getCertificate();
			privilegeHandler.addRoleToUser(certificate, BOB, ROLE_PRIVILEGE_ADMIN);
			logger.info("Added " + ROLE_PRIVILEGE_ADMIN + " to " + ADMIN);
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
			certificate = this.ctx.getCertificate();
			privilegeHandler.addUser(certificate, userRep, null);
			fail("User bob may not add a user as bob does not have admin rights!");
		} catch (PrivilegeException e) {
			String msg = MessageFormat.format(PrivilegeMessages.getString("Privilege.noprivilege.user"), //$NON-NLS-1$
					BOB, PrivilegeHandler.PRIVILEGE_ADD_USER);
			assertEquals(msg, e.getMessage());
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
			Certificate certificate = this.ctx.getCertificate();
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
			RoleRep roleRep = new RoleRep(ROLE_USER, new ArrayList<>());
			Certificate certificate = this.ctx.getCertificate();
			privilegeHandler.addRole(certificate, roleRep);
			privilegeHandler.persist(certificate);
		} finally {
			logout();
		}
	}

	private void enableBob() {
		try {
			// testEnableUserBob
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));
			Certificate certificate = this.ctx.getCertificate();
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
			fail("User Bob may not authenticate because the user is not yet enabled!");
		} catch (PrivilegeException e) {
			String msg = "User bob does not have state ENABLED and can not login!";
			assertEquals(msg, e.getMessage());
		} finally {
			logout();
		}
	}

	private void addBobAsAdmin() {
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));

			// let's add a new user bob
			UserRep userRep = new UserRep(null, BOB, "Bob", "Newman", UserState.NEW,
					new HashSet<>(Arrays.asList(ROLE_MY)), null, new HashMap<String, String>());
			Certificate certificate = this.ctx.getCertificate();
			privilegeHandler.addUser(certificate, userRep, null);
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
