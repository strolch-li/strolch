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

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.InvalidCredentialsException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.base.PrivilegeModelException;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.i18n.PrivilegeMessages;
import li.strolch.privilege.model.*;
import li.strolch.privilege.model.internal.UserChallenge;
import li.strolch.privilege.test.model.TestRestrictable;
import li.strolch.privilege.test.model.TestSystemUserAction;
import li.strolch.privilege.test.model.TestSystemUserActionDeny;
import li.strolch.privilege.test.model.TestUserChallengeHandler;
import li.strolch.utils.helper.ArraysHelper;
import org.hamcrest.MatcherAssert;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.text.MessageFormat;
import java.util.*;

import static li.strolch.utils.helper.ExceptionHelper.getRootCause;
import static li.strolch.utils.helper.ExceptionHelper.getRootCauseMessage;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.*;

/**
 * JUnit for performing Privilege tests. This JUnit is by no means complete, but checks the bare minimum.br />
 * <p>
 * TODO add more tests, especially with deny and allow lists
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class PrivilegeTest extends AbstractPrivilegeTest {

	private static final String ROLE_PRIVILEGE_ADMIN = "PrivilegeAdmin";
	private static final String PRIVILEGE_USER_ACCESS = "UserAccessPrivilege";
	private static final String ADMIN = "admin";
	private static final String ADMIN2 = "admin2";
	private static final char[] PASS_ADMIN = "admin".toCharArray();
	private static final String BOB = "bob";
	private static final String TED = "ted";
	private static final String SYSTEM_USER_ADMIN = "system_admin";
	private static final String SYSTEM_USER_ADMIN2 = "system_admin2";
	private static final char[] PASS_BOB = "admin1".toCharArray();
	private static final String ROLE_APP_USER = "AppUser";
	private static final String ROLE_MY = "MyRole";
	private static final String ROLE_MY2 = "MyRole2";
	private static final String ROLE_CHANGE_PW = "changePw";
	private static final String ROLE_TEMP = "temp";
	private static final String ROLE_USER = "user";
	private static final char[] PASS_DEF = "def".toCharArray();
	private static final char[] PASS_BAD = "123".toCharArray();
	private static final char[] PASS_TED = "12345".toCharArray();

	private static final Logger logger = LoggerFactory.getLogger(PrivilegeTest.class);

	@BeforeClass
	public static void init() {
		removeConfigs(PrivilegeTest.class.getSimpleName());
		prepareConfigs(PrivilegeTest.class.getSimpleName(), "PrivilegeConfig.xml", "PrivilegeUsers.xml",
				"PrivilegeGroups.xml", "PrivilegeRoles.xml");
	}

	@AfterClass
	public static void destroy() {
		//removeConfigs(PrivilegeTest.class.getSimpleName());
	}

	@Before
	public void setup() {
		initialize(PrivilegeTest.class.getSimpleName(), "PrivilegeConfig.xml");
	}

	@Test
	public void testAuthenticationOk() {
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));
		} finally {
			logout();
		}
	}

	@Test
	public void testAuthenticationAdmin2Ok() {
		try {
			login(ADMIN2, ArraysHelper.copyOf(PASS_ADMIN));
		} finally {
			logout();
		}
	}

	@Test
	public void testFailAuthenticationNOk() {
		InvalidCredentialsException exception = assertThrows(InvalidCredentialsException.class, () -> {
			try {
				login(ADMIN, ArraysHelper.copyOf(PASS_BAD));
			} finally {
				logout();
			}
		});
		assertEquals(InvalidCredentialsException.class, getRootCause(exception).getClass());
		MatcherAssert.assertThat(getRootCauseMessage(exception), containsString("Password is incorrect for admin"));
	}

	@Test
	public void testFailAuthenticationPWNull() {
		InvalidCredentialsException exception = assertThrows(InvalidCredentialsException.class, () -> {
			try {
				login(ADMIN, null);
			} finally {
				logout();
			}
		});
		assertEquals(InvalidCredentialsException.class, getRootCause(exception).getClass());
		MatcherAssert.assertThat(getRootCauseMessage(exception), containsString("Password is invalid!"));
	}

	@Test
	public void testAddRoleTemp() {
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));

			RoleRep roleRep = new RoleRep(ROLE_TEMP, Map.of());

			Certificate certificate = this.ctx.getCertificate();
			this.privilegeHandler.addRole(certificate, roleRep);
			this.privilegeHandler.persist(certificate);
		} finally {
			logout();
		}
	}

	@Test
	public void testPerformRestrictableAsAdmin() {
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
		this.privilegeHandler.runAs(SYSTEM_USER_ADMIN, action);
	}

	/**
	 * Checks that the system user can not perform a valid action, but illegal privilege
	 */
	@Test
	public void testPerformSystemRestrictableFailPrivilege() {
		PrivilegeException exception = assertThrows(PrivilegeException.class, () -> {
			try {
				// create the action to be performed as a system user
				TestSystemUserActionDeny action = new TestSystemUserActionDeny();

				// and then perform the action
				this.privilegeHandler.runAs(SYSTEM_USER_ADMIN, action);
			} finally {
				logout();
			}
		});
		MatcherAssert.assertThat(exception.getMessage(), containsString(
				"User system_admin does not have the privilege li.strolch.privilege.handler.SystemAction"));
	}

	/**
	 * Checks that the system user can not perform a valid action, but illegal privilege
	 */
	@Test
	public void testPerformSystemRestrictableFailNoAdditionalPrivilege() {
		PrivilegeException exception = assertThrows(PrivilegeException.class, () -> {
			try {
				// create the action to be performed as a system user
				TestSystemUserActionDeny action = new TestSystemUserActionDeny();

				// and then perform the action
				this.privilegeHandler.runAs(SYSTEM_USER_ADMIN2, action);
			} finally {
				logout();
			}
		});
		MatcherAssert.assertThat(exception.getMessage(), containsString(
				"User system_admin2 does not have the privilege li.strolch.privilege.handler.SystemAction with value "
						+ "li.strolch.privilege.test.model.TestSystemUserActionDeny needed for Restrictable "
						+ "li.strolch.privilege.test.model.TestSystemUserActionDeny"));
	}

	/**
	 * System user may not login
	 */
	@Test
	public void testLoginSystemUser() {
		AccessDeniedException exception = assertThrows(AccessDeniedException.class, () -> {
			try {
				login(SYSTEM_USER_ADMIN, SYSTEM_USER_ADMIN.toCharArray());
			} finally {
				logout();
			}
		});
		assertEquals(AccessDeniedException.class, getRootCause(exception).getClass());
		MatcherAssert.assertThat(getRootCauseMessage(exception),
				containsString("User system_admin is a system user and may not login!"));
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
			UserRep user = this.privilegeHandler.getUser(certificate, ADMIN);
			assertNotEquals("The", user.getFirstname());
			assertNotEquals("Admin", user.getLastname());

			// set new name
			user.setFirstname("The");
			user.setLastname("Admin");

			// update user
			this.privilegeHandler.updateUser(certificate, user, null);

			user = this.privilegeHandler.getUser(certificate, ADMIN);
			assertEquals("The", user.getFirstname());
			assertEquals("Admin", user.getLastname());

		} finally {
			logout();
		}
	}

	@Test
	public void shouldFailUpdateInexistantUser() {
		PrivilegeModelException exception = assertThrows(PrivilegeModelException.class, () -> {
			try {
				login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));

				Certificate certificate = this.ctx.getCertificate();

				// let's add a new user bob
				UserRep userRep = new UserRep(BOB, BOB, "Bob", "Anderson", UserState.ENABLED,
						Set.of("AppUserLocationA"), null, null, null, null);
				this.privilegeHandler.updateUser(certificate, userRep, null);
			} finally {
				logout();
			}
		});
		assertEquals(PrivilegeModelException.class, getRootCause(exception).getClass());
		MatcherAssert.assertThat(getRootCauseMessage(exception), containsString("User bob does not exist"));
	}

	@Test
	public void shouldQueryUsers() {
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));

			Certificate certificate = this.ctx.getCertificate();

			UserRep selectorRep = new UserRep(null, ADMIN, null, null, null, null, null, null, null, null);
			List<UserRep> users = this.privilegeHandler.queryUsers(certificate, selectorRep);
			assertEquals(1, users.size());
			assertEquals(ADMIN, users.getFirst().getUsername());

		} finally {
			logout();
		}
	}

	@Test
	public void shouldQueryUsersByGroups() {
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));

			Certificate certificate = this.ctx.getCertificate();

			UserRep selectorRep = new UserRep(null, null, null, null, null, Set.of("GroupA"), null, null, null, null);
			List<UserRep> users = this.privilegeHandler.queryUsers(certificate, selectorRep);
			assertEquals(1, users.size());
			assertEquals(ADMIN, users.getFirst().getUsername());

		} finally {
			logout();
		}
	}

	@Test
	public void shouldQueryUsersByRoles() {
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));

			Certificate certificate = this.ctx.getCertificate();

			UserRep selectorRep = new UserRep(null, null, null, null, null, null, Set.of("PrivilegeAdmin"), null, null,
					null);
			List<UserRep> users = this.privilegeHandler.queryUsers(certificate, selectorRep);
			assertEquals(2, users.size());
			assertEquals(ADMIN, users.getFirst().getUsername());

		} finally {
			logout();
		}
	}

	@Test
	public void shouldQueryUsersByRoles2() {
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));

			Certificate certificate = this.ctx.getCertificate();

			UserRep selectorRep = new UserRep(null, null, null, null, null,
					new HashSet<>(Collections.singletonList(ROLE_TEMP)), null, null, null, null);
			List<UserRep> users = this.privilegeHandler.queryUsers(certificate, selectorRep);
			assertEquals(0, users.size());

		} finally {
			logout();
		}
	}

	@Test
	public void shouldDetectPrivilegeConflict1() {
		PrivilegeModelException exception = assertThrows(PrivilegeModelException.class, () -> {
			try {
				login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));
				Certificate certificate = this.ctx.getCertificate();
				Privilege privilege = new Privilege(PrivilegeHandler.PRIVILEGE_ACTION, "DefaultPrivilege", true,
						Set.of(), Set.of());
				RoleRep role = this.privilegeHandler.getRole(certificate, ROLE_APP_USER);
				role.addPrivilege(privilege);
				this.privilegeHandler.replaceRole(certificate, role);
			} finally {
				logout();
			}
		});
		assertEquals(PrivilegeModelException.class, getRootCause(exception).getClass());
		MatcherAssert.assertThat(getRootCauseMessage(exception),
				containsString("User admin has conflicts for privilege "));
	}

	@Test
	public void shouldDetectPrivilegeConflict2() {
		PrivilegeModelException exception = assertThrows(PrivilegeModelException.class, () -> {
			try {
				login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));
				Certificate certificate = this.ctx.getCertificate();
				UserRep user = this.privilegeHandler.getUser(certificate, ADMIN);
				user.addRole(ROLE_MY);
				user.addRole(ROLE_MY2);
				this.privilegeHandler.updateUser(certificate, user, null);
			} finally {
				logout();
			}
		});
		assertEquals(PrivilegeModelException.class, getRootCause(exception).getClass());
		MatcherAssert.assertThat(getRootCauseMessage(exception),
				containsString("User admin has conflicts for privilege "));
	}

	@Test
	public void shouldSetPasswordWithChallenge() {
		this.privilegeHandler.initiateChallengeFor(Usage.SET_PASSWORD, ADMIN);
		UserChallenge userChallenge = TestUserChallengeHandler
				.getInstance()
				.getChallenges()
				.values()
				.stream()
				.filter(u -> u.getUser().getUsername().equals(ADMIN))
				.findFirst()
				.orElseThrow(() -> new IllegalStateException("Missing admin user!"));
		Certificate certificate = this.privilegeHandler.validateChallenge(ADMIN, userChallenge.getChallenge());
		this.privilegeHandler.setUserPassword(certificate, ADMIN, ArraysHelper.copyOf(PASS_TED));

		try {
			this.privilegeHandler.validate(certificate);
			fail("Certificate should not be valid anymore!");
		} catch (PrivilegeException e) {
			// expected
		}

		// change password back
		certificate = this.privilegeHandler.authenticate(ADMIN, PASS_TED, false);
		this.privilegeHandler.setUserPassword(certificate, ADMIN, ArraysHelper.copyOf(PASS_ADMIN));
		this.privilegeHandler.invalidate(certificate);
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
	public void testUserStory() {

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
				this.privilegeHandler.setUserState(certificate, TED, UserState.SYSTEM);
				fail("Should not be able to set user state to SYSTEM");
			} catch (AccessDeniedException e) {
				if (!(getRootCause(e) instanceof AccessDeniedException))
					fail("Unexpected root cause " + getRootCause(e));
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
				this.privilegeHandler.setUserPassword(certificate, BOB, ArraysHelper.copyOf(PASS_TED));
				fail("Should not be able to set password of other user, as missing privilege");
			} catch (AccessDeniedException e) {
				// ok
			}

			try {
				this.privilegeHandler.setUserLocale(certificate, BOB, Locale.FRENCH);
				fail("Should not be able to set locale of other user, as missing privilege");
			} catch (AccessDeniedException e) {
				if (!(getRootCause(e) instanceof AccessDeniedException))
					fail("Unexpected root cause " + getRootCause(e));
			}

			try {
				this.privilegeHandler.setUserState(certificate, BOB, UserState.DISABLED);
				fail("Should not be able to set state of other user, as missing privilege");
			} catch (AccessDeniedException e) {
				if (!(getRootCause(e) instanceof AccessDeniedException))
					fail("Unexpected root cause " + getRootCause(e));
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
				this.privilegeHandler.setUserPassword(certificate, TED, ArraysHelper.copyOf(PASS_TED));
				fail("Should not be able to set password, as missing privilege");
			} catch (AccessDeniedException e) {
				// ok
			}

			try {
				this.privilegeHandler.setUserLocale(certificate, TED, Locale.FRENCH);
				fail("Should not be able to set locale, as missing privilege");
			} catch (AccessDeniedException e) {
				if (!(getRootCause(e) instanceof AccessDeniedException))
					fail("Unexpected root cause " + getRootCause(e));
			}

			try {
				this.privilegeHandler.setUserState(certificate, TED, UserState.ENABLED);
				fail("Should not be able to set state, as missing privilege");
			} catch (AccessDeniedException e) {
				if (!(getRootCause(e) instanceof AccessDeniedException))
					fail("Unexpected root cause " + getRootCause(e));
			}

		} finally {
			logout();
		}
	}

	private void addChangePwRoleToTed() {
		try {
			// add role user
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));

			Privilege password = new Privilege(PrivilegeHandler.PRIVILEGE_SET_USER_PASSWORD, PRIVILEGE_USER_ACCESS,
					false, Set.of(), Set.of());
			Privilege locale = new Privilege(PrivilegeHandler.PRIVILEGE_SET_USER_LOCALE, PRIVILEGE_USER_ACCESS, false,
					Set.of(), Set.of());

			Map<String, Privilege> privileges = new HashMap<>();
			privileges.put(password.getName(), password);
			privileges.put(locale.getName(), locale);
			RoleRep roleRep = new RoleRep(ROLE_CHANGE_PW, privileges);

			Certificate certificate = this.ctx.getCertificate();
			this.privilegeHandler.addRole(certificate, roleRep);

			UserRep ted = this.privilegeHandler.getUser(certificate, TED);
			ted.addRole(ROLE_CHANGE_PW);
			this.privilegeHandler.updateUser(certificate, ted, null);
			logger.info("Added " + ROLE_CHANGE_PW + " to " + TED);
			this.privilegeHandler.persist(certificate);
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
			UserRep bob = this.privilegeHandler.getUser(certificate, BOB);
			bob.addRole(ROLE_APP_USER);
			this.privilegeHandler.updateUser(certificate, bob, null);
			logger.info("Added " + ROLE_APP_USER + " to " + BOB);
			this.privilegeHandler.persist(certificate);
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
			String msg =
					"User bob does not have the privilege li.strolch.privilege.test.model.TestRestrictable needed for "
							+ "Restrictable li.strolch.privilege.test.model.TestRestrictable and value "
							+ "li.strolch.privilege.test.model.TestRestrictable";
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
			this.privilegeHandler.setUserPassword(certificate, TED, ArraysHelper.copyOf(PASS_TED));
			this.privilegeHandler.setUserLocale(certificate, TED, Locale.FRENCH);
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
			this.privilegeHandler.setUserPassword(certificate, TED, ArraysHelper.copyOf(PASS_DEF));
			this.privilegeHandler.persist(certificate);
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
		} catch (AccessDeniedException e) {
			String msg = "User ted has no password and may not login!";
			assertEquals(InvalidCredentialsException.class, getRootCause(e).getClass());
			MatcherAssert.assertThat(getRootCauseMessage(e), containsString(msg));
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
			userRep = new UserRep(null, TED, "Ted", "Newman", UserState.ENABLED, Set.of(), roles, null, new HashMap<>(),
					null);
			Certificate certificate = this.ctx.getCertificate();
			this.privilegeHandler.addUser(certificate, userRep, null);
			logger.info("Added user " + TED);
			this.privilegeHandler.persist(certificate);
		} finally {
			logout();
		}
	}

	private void addRoleAdminToBob() {
		try {
			// testAddAdminRoleToBob
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));
			Certificate certificate = this.ctx.getCertificate();
			UserRep bob = this.privilegeHandler.getUser(certificate, BOB);
			bob.addRole(ROLE_PRIVILEGE_ADMIN);
			this.privilegeHandler.updateUser(certificate, bob, null);
			logger.info("Added " + ROLE_PRIVILEGE_ADMIN + " to " + ADMIN);
			this.privilegeHandler.persist(certificate);
		} finally {
			logout();
		}
	}

	private void failAddTedAsBobNotAdmin() {
		Certificate certificate;
		try {
			UserRep userRep;
			// testFailAddUserTedAsBob
			// Will fail because user bob does not have admin rights		
			// auth as Bob
			login(BOB, ArraysHelper.copyOf(PASS_BOB));
			// let's add a new user Ted
			userRep = new UserRep("1", TED, "Ted", "And then Some", UserState.NEW, Set.of(), Set.of(), null,
					new HashMap<>(), null);
			certificate = this.ctx.getCertificate();
			this.privilegeHandler.addUser(certificate, userRep, null);
			fail("User bob may not add a user as bob does not have admin rights!");
		} catch (AccessDeniedException e) {
			String msg = MessageFormat.format(PrivilegeMessages.getString("Privilege.noprivilege.user"), BOB,
					PrivilegeHandler.PRIVILEGE_ADD_USER);
			assertEquals(AccessDeniedException.class, getRootCause(e).getClass());
			MatcherAssert.assertThat(getRootCauseMessage(e), containsString(msg));
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
			UserRep bob = this.privilegeHandler.getUser(certificate, BOB);
			bob.addRole(ROLE_USER);
			this.privilegeHandler.updateUser(certificate, bob, null);
			this.privilegeHandler.persist(certificate);
			logout();
		} finally {
			logout();
		}
	}

	private void addRoleUser() {
		try {
			// add role user
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));
			RoleRep roleRep = new RoleRep(ROLE_USER, Map.of());
			Certificate certificate = this.ctx.getCertificate();
			this.privilegeHandler.addRole(certificate, roleRep);
			this.privilegeHandler.persist(certificate);
		} finally {
			logout();
		}
	}

	private void enableBob() {
		try {
			// testEnableUserBob
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));
			Certificate certificate = this.ctx.getCertificate();
			this.privilegeHandler.setUserState(certificate, BOB, UserState.ENABLED);
			this.privilegeHandler.persist(certificate);
		} finally {
			logout();
		}
	}

	private void failAuthAsBobNotEnabled() {
		try {
			// testFailAuthAsBob
			// Will fail because user bob is not yet enabled
			this.privilegeHandler.authenticate(BOB, ArraysHelper.copyOf(PASS_BOB), false);
			fail("User Bob may not authenticate because the user is not yet enabled!");
		} catch (AccessDeniedException e) {
			String msg = "User bob does not have state ENABLED and can not login!";
			assertEquals(AccessDeniedException.class, getRootCause(e).getClass());
			MatcherAssert.assertThat(getRootCauseMessage(e), containsString(msg));
		} finally {
			logout();
		}
	}

	private void addBobAsAdmin() {
		try {
			login(ADMIN, ArraysHelper.copyOf(PASS_ADMIN));

			// let's add a new user bob
			UserRep userRep = new UserRep(null, BOB, "Bob", "Newman", UserState.NEW, Set.of(), Set.of(ROLE_MY), null,
					new HashMap<>(), null);
			Certificate certificate = this.ctx.getCertificate();
			this.privilegeHandler.addUser(certificate, userRep, null);
			logger.info("Added user " + BOB);

			// set bob's password
			this.privilegeHandler.setUserPassword(certificate, BOB, ArraysHelper.copyOf(PASS_BOB));
			logger.info("Set Bob's password");
			this.privilegeHandler.persist(certificate);
		} finally {
			logout();
		}
	}
}
