/*
 * Copyright (c) 2026 Robert von Burg <eitch@eitchnet.ch>
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
import li.strolch.privilege.model.Group;
import li.strolch.privilege.model.RoleRep;
import li.strolch.privilege.model.UserRep;
import li.strolch.privilege.model.UserState;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.ZonedDateTime;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import static org.junit.Assert.*;

public class PrivilegeAutoPersistenceTest extends AbstractPrivilegeTest {

	private static final String TARGET_DST = PrivilegeAutoPersistenceTest.class.getSimpleName();
	private static final String BASE_PATH = "target/" + TARGET_DST;

	@Before
	public void init() {
		removeConfigs(TARGET_DST);
		prepareConfigs(TARGET_DST, "PrivilegeConfig.xml", "PrivilegeUsers.xml", "PrivilegeGroups.xml",
				"PrivilegeRoles.xml");
	}

	@AfterClass
	public static void destroy() {
		removeConfigs(TARGET_DST);
	}

	@After
	public void cleanup() {
		logout();
	}

	@Test
	public void shouldPersistCrudChangesWhenEnabled() throws Exception {
		initialize(true);
		login("admin", "admin".toCharArray());

		Path usersFile = Path.of(BASE_PATH, "PrivilegeUsers.xml");
		Path rolesFile = Path.of(BASE_PATH, "PrivilegeRoles.xml");
		Path groupsFile = Path.of(BASE_PATH, "PrivilegeGroups.xml");
		Path tokensFile = Path.of(BASE_PATH, "PrivilegeTokens.xml");
		assertThrows(AccessDeniedException.class,
				() -> this.privilegeHandler.persist(this.ctx.getCertificate()));
		assertTrue("Authentication should persist user history", Files.readString(usersFile).contains("<LastLogin>"));

		var certificate = this.ctx.getCertificate();
		UserRep user = new UserRep(null, "autopersist", "Auto", "Persist", UserState.ENABLED, Set.of(),
				Set.of("AppUser"), Locale.US, Map.of(), null);
		user = this.privilegeHandler.addUser(certificate, user, null);
		assertTrue(Files.readString(usersFile).contains("username=\"autopersist\""));

		user.setFirstname("Updated");
		this.privilegeHandler.updateUser(certificate, user, null);
		assertTrue(Files.readString(usersFile).contains("<Firstname>Updated</Firstname>"));

		this.privilegeHandler.setUserLocaleById(certificate, user.getUserId(), Locale.CANADA_FRENCH);
		assertTrue(Files.readString(usersFile).contains("<Locale>fr-CA</Locale>"));
		this.privilegeHandler.setUserStateById(certificate, user.getUserId(), UserState.DISABLED);
		assertTrue(Files.readString(usersFile).contains("<State>DISABLED</State>"));
		this.privilegeHandler.setUserPasswordById(certificate, user.getUserId(), "AutoPersist123!".toCharArray());
		this.privilegeHandler.requirePasswordChangeById(certificate, user.getUserId());
		assertTrue(Files.readString(usersFile).contains("<PasswordChangeRequested>true</PasswordChangeRequested>"));

		RoleRep role = new RoleRep("AutoPersistRole", Map.of());
		this.privilegeHandler.addRole(certificate, role);
		assertTrue(Files.readString(rolesFile).contains("AutoPersistRole"));
		this.privilegeHandler.replaceRole(certificate, role);

		Group group = new Group("AutoPersistGroup", Set.of("AppUser"), Map.of());
		this.privilegeHandler.addGroup(certificate, group);
		assertTrue(Files.readString(groupsFile).contains("AutoPersistGroup"));
		this.privilegeHandler.replaceGroup(certificate, group);

		ZonedDateTime now = ZonedDateTime.now();
		String token = this.privilegeHandler.createPersonalAccessToken(certificate, "AutoPersistToken",
				now.minusMinutes(1), now.plusDays(1), Set.of("AppUser"), Set.of());
		String tokenId = token.substring(0, token.indexOf(':'));
		assertTrue(Files.readString(tokensFile).contains(tokenId));
		this.privilegeHandler.removePersonalAccessToken(certificate, tokenId);
		assertFalse(Files.readString(tokensFile).contains(tokenId));

		this.privilegeHandler.removeGroup(certificate, group.name());
		assertFalse(Files.readString(groupsFile).contains("AutoPersistGroup"));
		this.privilegeHandler.removeRole(certificate, role.getName());
		assertFalse(Files.readString(rolesFile).contains("AutoPersistRole"));
		this.privilegeHandler.removeUser(certificate, user.getUsername());
		assertFalse(Files.readString(usersFile).contains("autopersist"));
	}

	@Test
	public void shouldNotPersistCrudChangesWhenDisabled() throws Exception {
		initialize(false);
		login("admin", "admin".toCharArray());

		Path usersFile = Path.of(BASE_PATH, "PrivilegeUsers.xml");
		assertFalse("Authentication should not persist user history when disabled",
				Files.readString(usersFile).contains("<LastLogin>"));
		UserRep user = new UserRep(null, "notpersisted", "Not", "Persisted", UserState.ENABLED, Set.of(),
				Set.of("AppUser"), Locale.US, Map.of(), null);
		this.privilegeHandler.addUser(this.ctx.getCertificate(), user, null);

		assertFalse(Files.readString(usersFile).contains("notpersisted"));
	}

	private void initialize(boolean autoPersist) throws Exception {
		File configFile = getPrivilegeConfigFile(TARGET_DST, "PrivilegeConfig.xml");
		String config = Files.readString(configFile.toPath());
		config = config.replaceAll("(<Parameter name=\"persistSessions\" value=\")(?:true|false)(\"/>)",
				"$1false$2");
		config = config.replaceAll("(<Parameter name=\"autoPersistOnUserChangesData\" value=\")(?:true|false)(\"/>)",
				"$1" + autoPersist + "$2");
		Files.writeString(configFile.toPath(), config);

		File rolesFile = getPrivilegeConfigFile(TARGET_DST, "PrivilegeRoles.xml");
		String roles = Files.readString(rolesFile.toPath()).replace("<Allow>Persist</Allow>", "");
		Files.writeString(rolesFile.toPath(), roles);

		super.initialize(TARGET_DST, "PrivilegeConfig.xml");
	}
}
