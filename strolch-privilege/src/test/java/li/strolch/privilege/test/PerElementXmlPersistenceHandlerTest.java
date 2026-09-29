/*
 * Copyright (c) 2013-2026 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.privilege.handler.PerElementXmlPersistenceHandler;
import li.strolch.privilege.model.*;
import li.strolch.privilege.model.internal.*;
import li.strolch.privilege.xml.CertificateStubsSaxReader.CertificateStub;
import li.strolch.privilege.xml.CertificateStubsSaxWriter;
import li.strolch.utils.helper.FileHelper;
import li.strolch.utils.iso8601.ISO8601;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

import static li.strolch.privilege.helper.XmlConstants.*;
import static org.junit.Assert.*;

public class PerElementXmlPersistenceHandlerTest extends AbstractPrivilegeTest {

	private static final String TARGET_DST = PerElementXmlPersistenceHandlerTest.class.getSimpleName();
	private static final String BASE_PATH = "target/" + TARGET_DST;

	@BeforeClass
	public static void init() {
		removeConfigs(TARGET_DST);
		prepareConfigs(TARGET_DST, "PrivilegeConfigPerElement.xml", "PrivilegeUsers.xml", "PrivilegeGroups.xml",
				"PrivilegeRoles.xml");

		// Also copy PrivilegeTokens.xml for migration testing
		try {
			File srcTokens = new File(SRC_TEST_RESOURCES_CONFIG, "PrivilegeTokens.xml");
			File dstTokens = new File(BASE_PATH, "PrivilegeTokens.xml");
			if (srcTokens.exists())
				Files.copy(srcTokens.toPath(), dstTokens.toPath());
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	@AfterClass
	public static void destroy() {
		removeConfigs(TARGET_DST);
	}

	@Before
	public void setup() {
		initialize(TARGET_DST, "PrivilegeConfigPerElement.xml");
	}

	@Test
	public void shouldAutoMigrateMonolithicFiles() {
		File modelDir = new File(BASE_PATH, "model");
		File stateDir = new File(BASE_PATH, "state");

		assertTrue("model directory should exist", modelDir.exists());
		assertTrue("state directory should exist", stateDir.exists());

		File usersModelDir = new File(modelDir, "users");
		File rolesModelDir = new File(modelDir, "roles");
		File groupsModelDir = new File(modelDir, "groups");
		File tokensModelDir = new File(modelDir, "tokens");

		File usersStateDir = new File(stateDir, "users");
		File tokensStateDir = new File(stateDir, "tokens");

		assertTrue("model/users should exist", usersModelDir.exists());
		assertTrue("model/roles should exist", rolesModelDir.exists());
		assertTrue("model/groups should exist", groupsModelDir.exists());
		assertTrue("model/tokens should exist", tokensModelDir.exists());
		assertTrue("state/users should exist", usersStateDir.exists());
		assertTrue("state/tokens should exist", tokensStateDir.exists());

		// Verify model files exist
		assertTrue(new File(usersModelDir, "1.xml").exists());
		assertTrue(new File(usersModelDir, "2.xml").exists());
		assertTrue(new File(usersModelDir, "3.xml").exists());

		assertTrue(new File(rolesModelDir, "PrivilegeAdmin.xml").exists());
		assertTrue(new File(rolesModelDir, "AppUser.xml").exists());

		assertTrue(new File(groupsModelDir, "GroupA.xml").exists());

		// Verify tokens migration if tokens file was present
		File tokenModelFile = new File(tokensModelDir, "50b31270-bc49-4940-97ec-d4aa0d1ad649.xml");
		if (tokenModelFile.exists()) {
			File tokenStateFile = new File(tokensStateDir, "50b31270-bc49-4940-97ec-d4aa0d1ad649.properties");
			assertTrue(tokenStateFile.exists());
		}
	}

	@Test
	public void shouldAuthenticateAndHydrateState() {
		Certificate cert = privilegeHandler.authenticate("admin", "admin".toCharArray(), false);
		assertNotNull(cert);
		PrivilegeContext ctx = privilegeHandler.validate(cert);
		assertNotNull(ctx);
		assertEquals("admin", ctx.getUsername());
		privilegeHandler.invalidate(cert);
	}

	@Test
	public void shouldIsolateRuntimeStateFromStaticModel() throws IOException {
		PerElementXmlPersistenceHandler handler = new PerElementXmlPersistenceHandler();
		Map<String, String> params = Map.of(PARAM_BASE_PATH, BASE_PATH, PARAM_MODEL_DIR, "model", PARAM_STATE_DIR,
				"state", PARAM_AUTO_MIGRATE_MONOLITHIC, "false");
		handler.initialize(params);

		User admin = handler.getUser("admin");
		assertNotNull(admin);

		File adminXml = new File(BASE_PATH + "/model/users", admin.getUserId() + ".xml");
		File adminState = new File(BASE_PATH + "/state/users", admin.getUserId() + ".properties");

		assertTrue(adminXml.exists());
		byte[] xmlBytesBefore = Files.readAllBytes(adminXml.toPath());
		long xmlModifiedBefore = adminXml.lastModified();

		// Update only user history (login update)
		ZonedDateTime now = ZonedDateTime.now().truncatedTo(ChronoUnit.MILLIS);
		UserHistory updatedHistory = admin.getHistory().withLogin(now);
		User updatedUser = admin.withHistory(updatedHistory);
		handler.replaceUser(updatedUser);

		// Verify XML was NOT changed
		byte[] xmlBytesAfter = Files.readAllBytes(adminXml.toPath());
		long xmlModifiedAfter = adminXml.lastModified();
		assertEquals("XML content must remain untouched when only history changes", new String(xmlBytesBefore),
				new String(xmlBytesAfter));
		assertEquals("XML last modified must remain untouched", xmlModifiedBefore, xmlModifiedAfter);

		// Verify state file was updated
		assertTrue(adminState.exists());
		Properties adminProps = new Properties();
		try (var in = Files.newInputStream(adminState.toPath())) {
			adminProps.load(in);
		}
		assertTrue(adminProps.containsKey(PROP_LAST_LOGIN));
		assertEquals(ISO8601.toString(now), adminProps.getProperty(PROP_LAST_LOGIN));

		// Verify reload hydrates history
		PerElementXmlPersistenceHandler reloadHandler = new PerElementXmlPersistenceHandler();
		reloadHandler.initialize(params);
		User reloadedAdmin = reloadHandler.getUser("admin");
		assertNotNull(reloadedAdmin);
		assertEquals(now, reloadedAdmin.getHistory().getLastLogin());
	}

	@Test
	public void shouldUpdateUserStateDirectly() throws IOException {
		PerElementXmlPersistenceHandler handler = new PerElementXmlPersistenceHandler();
		Map<String, String> params = Map.of(PARAM_BASE_PATH, BASE_PATH, PARAM_MODEL_DIR, "model", PARAM_STATE_DIR,
				"state", PARAM_AUTO_MIGRATE_MONOLITHIC, "false");
		handler.initialize(params);

		User admin = handler.getUser("admin");
		assertNotNull(admin);

		File adminXml = new File(BASE_PATH + "/model/users", admin.getUserId() + ".xml");
		File adminState = new File(BASE_PATH + "/state/users", admin.getUserId() + ".properties");

		assertTrue(adminXml.exists());
		byte[] xmlBytesBefore = Files.readAllBytes(adminXml.toPath());
		long xmlModifiedBefore = adminXml.lastModified();

		// Update user state directly via updateUserState()
		ZonedDateTime now = ZonedDateTime.now().truncatedTo(ChronoUnit.MILLIS);
		UserHistory updatedHistory = admin.getHistory().withLogin(now);
		User updatedUser = admin.withHistory(updatedHistory);
		boolean updated = handler.updateUserState(updatedUser);
		assertTrue(updated);

		// Verify XML was NOT changed
		byte[] xmlBytesAfter = Files.readAllBytes(adminXml.toPath());
		long xmlModifiedAfter = adminXml.lastModified();
		assertEquals("XML content must remain untouched during updateUserState", new String(xmlBytesBefore),
				new String(xmlBytesAfter));
		assertEquals("XML last modified must remain untouched", xmlModifiedBefore, xmlModifiedAfter);

		// Verify state file was updated
		assertTrue(adminState.exists());
		Properties adminProps = new Properties();
		try (var in = Files.newInputStream(adminState.toPath())) {
			adminProps.load(in);
		}
		assertTrue(adminProps.containsKey(PROP_LAST_LOGIN));
		assertEquals(ISO8601.toString(now), adminProps.getProperty(PROP_LAST_LOGIN));

		// Verify reload hydrates history
		PerElementXmlPersistenceHandler reloadHandler = new PerElementXmlPersistenceHandler();
		reloadHandler.initialize(params);
		User reloadedAdmin = reloadHandler.getUser("admin");
		assertNotNull(reloadedAdmin);
		assertEquals(now, reloadedAdmin.getHistory().getLastLogin());
	}

	@Test
	public void shouldIsolateTokenRuntimeStateFromStaticModel() throws IOException {
		PerElementXmlPersistenceHandler handler = new PerElementXmlPersistenceHandler();
		Map<String, String> params = Map.of(PARAM_BASE_PATH, BASE_PATH, PARAM_MODEL_DIR, "model", PARAM_STATE_DIR,
				"state", PARAM_AUTO_MIGRATE_MONOLITHIC, "false");
		handler.initialize(params);

		String tokenId = UUID.randomUUID().toString();
		ZonedDateTime now = ZonedDateTime.now().truncatedTo(ChronoUnit.MILLIS);
		PersonalAccessToken token = new PersonalAccessToken(tokenId, "admin", "Test Isolation Token",
				new PasswordCrypt("pwd".getBytes(), "salt".getBytes(), "PBKDF2WithHmacSHA512", 10000, 256), now,
				now.plusDays(10), null, Map.of());
		handler.addAccessToken(token);

		File tokenXml = new File(BASE_PATH + "/model/tokens", tokenId + ".xml");
		File tokenState = new File(BASE_PATH + "/state/tokens", tokenId + ".properties");

		assertTrue(tokenXml.exists());
		byte[] xmlBytesBefore = Files.readAllBytes(tokenXml.toPath());
		long xmlModifiedBefore = tokenXml.lastModified();

		// Update token last used
		ZonedDateTime lastUsed = now.plusHours(1);
		boolean updated = handler.updateAccessTokenLastUsed(tokenId, lastUsed);
		assertTrue(updated);

		// Verify XML was NOT modified
		byte[] xmlBytesAfter = Files.readAllBytes(tokenXml.toPath());
		long xmlModifiedAfter = tokenXml.lastModified();
		assertEquals("Token XML content must remain untouched when lastUsed changes", new String(xmlBytesBefore),
				new String(xmlBytesAfter));
		assertEquals("Token XML last modified must remain untouched", xmlModifiedBefore, xmlModifiedAfter);

		// Verify state file was created and contains lastUsed
		assertTrue(tokenState.exists());
		Properties tokenProps = new Properties();
		try (var in = Files.newInputStream(tokenState.toPath())) {
			tokenProps.load(in);
		}
		assertTrue(tokenProps.containsKey(PROP_LAST_USED));
		assertEquals(ISO8601.toString(lastUsed), tokenProps.getProperty(PROP_LAST_USED));

		// Clean up
		handler.removeAccessToken(tokenId);
		assertFalse(tokenXml.exists());
		assertFalse(tokenState.exists());
	}

	@Test
	public void shouldPerformModelCrudOperations() throws IOException {
		PerElementXmlPersistenceHandler handler = new PerElementXmlPersistenceHandler();
		Map<String, String> params = Map.of(PARAM_BASE_PATH, BASE_PATH, PARAM_MODEL_DIR, "model", PARAM_STATE_DIR,
				"state", PARAM_AUTO_MIGRATE_MONOLITHIC, "false");
		handler.initialize(params);

		// 1. Role CRUD
		Role newRole = new Role("CustomRole",
				Map.of("priv1", new Privilege("priv1", "DefaultPrivilege", true, Set.of(), Set.of())));
		handler.addRole(newRole);
		File roleFile = new File(BASE_PATH + "/model/roles", "CustomRole.xml");
		assertTrue(roleFile.exists());
		assertEquals(newRole, handler.getRole("CustomRole"));

		Role updatedRole = new Role("CustomRole",
				Map.of("priv2", new Privilege("priv2", "DefaultPrivilege", true, Set.of(), Set.of())));
		handler.replaceRole(updatedRole);
		assertEquals(updatedRole, handler.getRole("CustomRole"));

		Role removedRole = handler.removeRole("CustomRole");
		assertEquals(updatedRole, removedRole);
		assertFalse(roleFile.exists());
		assertNull(handler.getRole("CustomRole"));

		// 2. Group CRUD
		Group newGroup = new Group("CustomGroup", Set.of("AppUser"), Map.of("prop1", "val1"));
		handler.addGroup(newGroup);
		File groupFile = new File(BASE_PATH + "/model/groups", "CustomGroup.xml");
		assertTrue(groupFile.exists());
		assertEquals(newGroup, handler.getGroup("CustomGroup"));

		Group updatedGroup = new Group("CustomGroup", Set.of("AppUser", "PrivilegeAdmin"), Map.of("prop1", "val2"));
		handler.replaceGroup(updatedGroup);
		assertEquals(updatedGroup, handler.getGroup("CustomGroup"));

		Group removedGroup = handler.removeGroup("CustomGroup");
		assertEquals(updatedGroup, removedGroup);
		assertFalse(groupFile.exists());
		assertNull(handler.getGroup("CustomGroup"));

		// 3. User CRUD
		String newUserId = UUID.randomUUID().toString();
		User newUser = new User(newUserId, "testuser",
				new PasswordCrypt("pwd".getBytes(), "salt".getBytes(), "PBKDF2WithHmacSHA512", 10000, 256), "Test",
				"User", UserState.ENABLED, Set.of(), Set.of("AppUser"), Locale.ENGLISH, Map.of(), false,
				UserHistory.EMPTY);
		handler.addUser(newUser);

		File userXml = new File(BASE_PATH + "/model/users", newUserId + ".xml");
		assertTrue(userXml.exists());
		assertEquals(newUser, handler.getUser("testuser"));
		assertEquals(newUser, handler.getUserById(newUserId));

		// Replace static config
		User updatedUser = new User(newUserId, "testuser", newUser.getPasswordCrypt(), "Updated", "Name",
				UserState.ENABLED, Set.of(), Set.of("AppUser"), Locale.ENGLISH, Map.of("key", "val"), false,
				UserHistory.EMPTY);
		handler.replaceUser(updatedUser);
		assertEquals("Updated", handler.getUser("testuser").getFirstname());

		User removedUser = handler.removeUserById(newUserId);
		assertEquals(updatedUser, removedUser);
		assertFalse(userXml.exists());
		assertNull(handler.getUser("testuser"));
		assertNull(handler.getUserById(newUserId));
	}

	@Test
	public void shouldHandleConcurrentLoginsWithoutContention() throws Exception {
		PerElementXmlPersistenceHandler handler = new PerElementXmlPersistenceHandler();
		Map<String, String> params = Map.of(PARAM_BASE_PATH, BASE_PATH, PARAM_MODEL_DIR, "model", PARAM_STATE_DIR,
				"state", PARAM_AUTO_MIGRATE_MONOLITHIC, "false");
		handler.initialize(params);

		// Create 10 distinct users
		List<User> createdUsers = new ArrayList<>();
		for (int i = 0; i < 10; i++) {
			String userId = "concurrent_user_" + i;
			String username = "user_" + i;
			User user = new User(userId, username,
					new PasswordCrypt("pwd".getBytes(), "salt".getBytes(), "PBKDF2WithHmacSHA512", 10000, 256), "First",
					"Last", UserState.ENABLED, Set.of(), Set.of("AppUser"), Locale.ENGLISH, Map.of(), false,
					UserHistory.EMPTY);
			handler.addUser(user);
			createdUsers.add(user);
		}

		int threadCount = 10;
		int iterationsPerThread = 50;
		ExecutorService executor = Executors.newFixedThreadPool(threadCount);
		CountDownLatch startLatch = new CountDownLatch(1);
		List<Future<Void>> futures = new ArrayList<>();

		for (int t = 0; t < threadCount; t++) {
			final int userIndex = t;
			Callable<Void> task = () -> {
				startLatch.await();
				User user = createdUsers.get(userIndex);
				for (int iter = 0; iter < iterationsPerThread; iter++) {
					ZonedDateTime loginTime = ZonedDateTime.now().truncatedTo(ChronoUnit.MILLIS);
					User current = handler.getUserById(user.getUserId());
					handler.replaceUser(current.withHistory(current.getHistory().withLogin(loginTime)));
				}
				return null;
			};
			futures.add(executor.submit(task));
		}

		startLatch.countDown();
		for (Future<Void> future : futures) {
			future.get(30, TimeUnit.SECONDS);
		}
		executor.shutdown();

		// Verify state for all users
		for (User user : createdUsers) {
			User reloaded = handler.getUserById(user.getUserId());
			assertNotNull(reloaded);
			assertFalse(reloaded.isHistoryEmpty());
			assertFalse(reloaded.getHistory().isLastLoginEmpty());

			File stateFile = new File(BASE_PATH + "/state/users", user.getUserId() + ".properties");
			assertTrue(stateFile.exists());

			// Clean up
			handler.removeUserById(user.getUserId());
			assertFalse(stateFile.exists());
		}
	}

	@Test
	public void shouldHandleConcurrentTokenUsageUpdates() throws Exception {
		PerElementXmlPersistenceHandler handler = new PerElementXmlPersistenceHandler();
		Map<String, String> params = Map.of(PARAM_BASE_PATH, BASE_PATH, PARAM_MODEL_DIR, "model", PARAM_STATE_DIR,
				"state", PARAM_AUTO_MIGRATE_MONOLITHIC, "false");
		handler.initialize(params);

		// Create 10 distinct tokens
		List<PersonalAccessToken> createdTokens = new ArrayList<>();
		ZonedDateTime now = ZonedDateTime.now().truncatedTo(ChronoUnit.MILLIS);
		for (int i = 0; i < 10; i++) {
			String tokenId = "concurrent_token_" + i;
			PersonalAccessToken token = new PersonalAccessToken(tokenId, "admin", "Token " + i,
					new PasswordCrypt("pwd".getBytes(), "salt".getBytes(), "PBKDF2WithHmacSHA512", 10000, 256), now,
					now.plusDays(10), null, Map.of());
			handler.addAccessToken(token);
			createdTokens.add(token);
		}

		int threadCount = 10;
		int iterationsPerThread = 50;
		ExecutorService executor = Executors.newFixedThreadPool(threadCount);
		CountDownLatch startLatch = new CountDownLatch(1);
		List<Future<Void>> futures = new ArrayList<>();

		for (int t = 0; t < threadCount; t++) {
			final int tokenIndex = t;
			Callable<Void> task = () -> {
				startLatch.await();
				String tokenId = createdTokens.get(tokenIndex).tokenId();
				for (int iter = 0; iter < iterationsPerThread; iter++) {
					ZonedDateTime usageTime = ZonedDateTime.now().truncatedTo(ChronoUnit.MILLIS);
					boolean updated = handler.updateAccessTokenLastUsed(tokenId, usageTime);
					assertTrue(updated);
				}
				return null;
			};
			futures.add(executor.submit(task));
		}

		startLatch.countDown();
		for (Future<Void> future : futures) {
			future.get(30, TimeUnit.SECONDS);
		}
		executor.shutdown();

		// Verify state for all tokens
		for (PersonalAccessToken token : createdTokens) {
			PersonalAccessToken reloaded = handler.getAccessToken(token.tokenId());
			assertNotNull(reloaded);
			assertNotNull(reloaded.lastUsed());

			File stateFile = new File(BASE_PATH + "/state/tokens", token.tokenId() + ".properties");
			assertTrue(stateFile.exists());

			// Clean up
			handler.removeAccessToken(token.tokenId());
			assertFalse(stateFile.exists());
		}
	}

	@Test
	public void shouldHandleContentionOnSameUser() throws Exception {
		PerElementXmlPersistenceHandler handler = new PerElementXmlPersistenceHandler();
		Map<String, String> params = Map.of(PARAM_BASE_PATH, BASE_PATH, PARAM_MODEL_DIR, "model", PARAM_STATE_DIR,
				"state", PARAM_AUTO_MIGRATE_MONOLITHIC, "false");
		handler.initialize(params);

		String userId = "contended_user";
		User user = new User(userId, "contended_user",
				new PasswordCrypt("pwd".getBytes(), "salt".getBytes(), "PBKDF2WithHmacSHA512", 10000, 256), "Contended",
				"User", UserState.ENABLED, Set.of(), Set.of("AppUser"), Locale.ENGLISH, Map.of(), false,
				UserHistory.EMPTY);
		handler.addUser(user);

		int threadCount = 8;
		int iterations = 30;
		ExecutorService executor = Executors.newFixedThreadPool(threadCount);
		CountDownLatch startLatch = new CountDownLatch(1);
		List<Future<Void>> futures = new ArrayList<>();

		for (int t = 0; t < threadCount; t++) {
			Callable<Void> task = () -> {
				startLatch.await();
				for (int i = 0; i < iterations; i++) {
					ZonedDateTime now = ZonedDateTime.now().truncatedTo(ChronoUnit.MILLIS);
					User current = handler.getUserById(userId);
					handler.replaceUser(current.withHistory(current.getHistory().withLogin(now)));
				}
				return null;
			};
			futures.add(executor.submit(task));
		}

		startLatch.countDown();
		for (Future<Void> future : futures) {
			future.get(30, TimeUnit.SECONDS);
		}
		executor.shutdown();

		User finalUser = handler.getUserById(userId);
		assertNotNull(finalUser);
		assertFalse(finalUser.isHistoryEmpty());

		handler.removeUserById(userId);
	}

	@Test
	public void shouldValidateDuplicatesAndReferences() {
		PerElementXmlPersistenceHandler handler = new PerElementXmlPersistenceHandler();
		Map<String, String> params = Map.of(PARAM_BASE_PATH, BASE_PATH, PARAM_MODEL_DIR, "model", PARAM_STATE_DIR,
				"state", PARAM_AUTO_MIGRATE_MONOLITHIC, "false");
		handler.initialize(params);

		// Duplicate username
		User duplicateUsername = new User("unique_id_999", "admin", null, "A", "B", UserState.ENABLED, Set.of(),
				Set.of("AppUser"), Locale.ENGLISH, Map.of(), false, UserHistory.EMPTY);
		assertThrows(IllegalStateException.class, () -> handler.addUser(duplicateUsername));

		// Duplicate userId
		User duplicateUserId = new User("1", "unique_username_999", null, "A", "B", UserState.ENABLED, Set.of(),
				Set.of("AppUser"), Locale.ENGLISH, Map.of(), false, UserHistory.EMPTY);
		assertThrows(IllegalStateException.class, () -> handler.addUser(duplicateUserId));

		// Duplicate role
		Role duplicateRole = new Role("PrivilegeAdmin", Map.of());
		assertThrows(IllegalStateException.class, () -> handler.addRole(duplicateRole));

		// Duplicate group
		Group duplicateGroup = new Group("GroupA", Set.of(), Map.of());
		assertThrows(IllegalStateException.class, () -> handler.addGroup(duplicateGroup));
	}

	@Test
	public void shouldHandleSpecialCharactersInIdentifiers() {
		PerElementXmlPersistenceHandler handler = new PerElementXmlPersistenceHandler();
		Map<String, String> params = Map.of(PARAM_BASE_PATH, BASE_PATH, PARAM_MODEL_DIR, "model", PARAM_STATE_DIR,
				"state", PARAM_AUTO_MIGRATE_MONOLITHIC, "false");
		handler.initialize(params);

		String specialUserId = "domain\\user@example.com";
		User user = new User(specialUserId, "special_user",
				new PasswordCrypt("pwd".getBytes(), "salt".getBytes(), "PBKDF2WithHmacSHA512", 10000, 256), "Special",
				"User", UserState.ENABLED, Set.of(), Set.of("AppUser"), Locale.ENGLISH, Map.of(), false,
				UserHistory.EMPTY);
		handler.addUser(user);

		String safeFilename = FileHelper.toSafeFilename(specialUserId);
		File userXml = new File(BASE_PATH + "/model/users", safeFilename + ".xml");
		assertTrue("File with sanitized filename should exist: " + userXml.getName(), userXml.exists());

		User fetched = handler.getUserById(specialUserId);
		assertEquals(user, fetched);

		handler.removeUserById(specialUserId);
		assertFalse(userXml.exists());
	}

	@Test
	public void shouldHandleNonExistentTokenUpdate() {
		PerElementXmlPersistenceHandler handler = new PerElementXmlPersistenceHandler();
		Map<String, String> params = Map.of(PARAM_BASE_PATH, BASE_PATH, PARAM_MODEL_DIR, "model", PARAM_STATE_DIR,
				"state", PARAM_AUTO_MIGRATE_MONOLITHIC, "false");
		handler.initialize(params);

		boolean updated = handler.updateAccessTokenLastUsed("non-existent-token-id", ZonedDateTime.now());
		assertFalse(updated);
	}

	@Test
	public void shouldRemoveOrphanTokensOnReload() {
		PerElementXmlPersistenceHandler handler = new PerElementXmlPersistenceHandler();
		Map<String, String> params = Map.of(PARAM_BASE_PATH, BASE_PATH, PARAM_MODEL_DIR, "model", PARAM_STATE_DIR,
				"state", PARAM_AUTO_MIGRATE_MONOLITHIC, "false");
		handler.initialize(params);

		String orphanTokenId = UUID.randomUUID().toString();
		ZonedDateTime now = ZonedDateTime.now().truncatedTo(ChronoUnit.MILLIS);
		PersonalAccessToken orphanToken = new PersonalAccessToken(orphanTokenId, "non_existent_user_999",
				"Orphan Token",
				new PasswordCrypt("pwd".getBytes(), "salt".getBytes(), "PBKDF2WithHmacSHA512", 10000, 256), now,
				now.plusDays(10), null, Map.of());
		handler.addAccessToken(orphanToken);
		assertNotNull(handler.getAccessToken(orphanTokenId));

		// Reload should detect that non_existent_user_999 does not exist and remove orphan token from in-memory map
		handler.reload();
		assertNull("Orphan token should be removed upon reload", handler.getAccessToken(orphanTokenId));

		// Clean up file
		handler.removeAccessToken(orphanTokenId);
	}

	@Test
	public void shouldSupportConcurrentReadsWhileWriting() throws Exception {
		PerElementXmlPersistenceHandler handler = new PerElementXmlPersistenceHandler();
		Map<String, String> params = Map.of(PARAM_BASE_PATH, BASE_PATH, PARAM_MODEL_DIR, "model", PARAM_STATE_DIR,
				"state", PARAM_AUTO_MIGRATE_MONOLITHIC, "false");
		handler.initialize(params);

		String writerUserId = "reader_writer_user";
		User user = new User(writerUserId, "rw_user",
				new PasswordCrypt("pwd".getBytes(), "salt".getBytes(), "PBKDF2WithHmacSHA512", 10000, 256), "RW",
				"User", UserState.ENABLED, Set.of(), Set.of("AppUser"), Locale.ENGLISH, Map.of(), false,
				UserHistory.EMPTY);
		handler.addUser(user);

		int readersCount = 4;
		int writersCount = 2;
		int iterations = 100;
		ExecutorService executor = Executors.newFixedThreadPool(readersCount + writersCount);
		CountDownLatch latch = new CountDownLatch(1);
		AtomicInteger readErrors = new AtomicInteger(0);
		List<Future<Void>> futures = new ArrayList<>();

		// Readers
		for (int r = 0; r < readersCount; r++) {
			futures.add(executor.submit(() -> {
				latch.await();
				for (int i = 0; i < iterations; i++) {
					User u = handler.getUser("rw_user");
					if (u == null || !u.getUsername().equals("rw_user")) {
						readErrors.incrementAndGet();
					}
					List<User> all = handler.getAllUsers();
					if (all.isEmpty()) {
						readErrors.incrementAndGet();
					}
				}
				return null;
			}));
		}

		// Writers
		for (int w = 0; w < writersCount; w++) {
			futures.add(executor.submit(() -> {
				latch.await();
				for (int i = 0; i < iterations; i++) {
					ZonedDateTime now = ZonedDateTime.now().truncatedTo(ChronoUnit.MILLIS);
					User curr = handler.getUserById(writerUserId);
					if (curr != null) {
						handler.updateUserState(curr.withHistory(curr.getHistory().withLogin(now)));
					}
				}
				return null;
			}));
		}

		latch.countDown();
		for (Future<Void> future : futures) {
			future.get(30, TimeUnit.SECONDS);
		}
		executor.shutdown();

		assertEquals(0, readErrors.get());
		handler.removeUserById(writerUserId);
	}

	@Test
	public void shouldPersistAndReloadSessionsPerElement() throws Exception {
		File sessionsStateDir = new File(BASE_PATH, "state/sessions");
		assertTrue("State sessions dir should exist", sessionsStateDir.exists());

		// Login and assert session properties file is created
		login("admin", "admin".toCharArray());
		Certificate cert = ctx.getCertificate();
		File sessionFile = new File(sessionsStateDir, cert.getSessionId() + ".properties");
		assertTrue("Session file should be created: " + sessionFile.getAbsolutePath(), sessionFile.exists());

		// Read properties to verify content
		Properties props = new Properties();
		try (InputStream in = Files.newInputStream(sessionFile.toPath())) {
			props.load(in);
		}
		assertEquals(cert.getSessionId(), props.getProperty(PROP_SESSION_ID));
		assertEquals("admin", props.getProperty(PROP_USERNAME));
		assertEquals(Usage.ANY.name(), props.getProperty(PROP_USAGE));
		assertNotNull(props.getProperty(PROP_AUTH_TOKEN));
		assertNotNull(props.getProperty(PROP_LOGIN_TIME));
		assertNotNull(props.getProperty(PROP_LAST_ACCESS));

		// Validate and verify update of lastAccess
		Thread.sleep(50L);
		this.privilegeHandler.validate(cert);
		Properties updatedProps = new Properties();
		try (InputStream in = Files.newInputStream(sessionFile.toPath())) {
			updatedProps.load(in);
		}
		assertNotNull(updatedProps.getProperty(PROP_LAST_ACCESS));

		// Re-initialize and verify session restored
		initialize(TARGET_DST, "PrivilegeConfigPerElement.xml");
		PrivilegeContext restoredCtx = this.privilegeHandler.validate(cert);
		assertNotNull("Restored session must be valid", restoredCtx);
		assertEquals("admin", restoredCtx.getUsername());

		// Invalidate and verify file deletion
		this.privilegeHandler.invalidate(cert);
		assertFalse("Session file should be deleted on invalidation", sessionFile.exists());
	}

	@Test
	public void shouldAutoMigrateSessionsFromMonolithicXml() throws Exception {
		String testTarget = "PerElementSessionMigrationTest";
		String testBasePath = "target/" + testTarget;
		removeConfigs(testTarget);
		prepareConfigs(testTarget, "PrivilegeConfigPerElement.xml", "PrivilegeUsers.xml", "PrivilegeGroups.xml",
				"PrivilegeRoles.xml");

		// Write a monolithic PrivilegeSessions.xml in base path
		File monolithicSessions = new File(testBasePath, "PrivilegeSessions.xml");
		List<CertificateStub> stubs = List.of(
				new CertificateStub(Usage.ANY, "migrated-session-123", "admin", "token-abc-123", "127.0.0.1",
						Locale.ENGLISH, ZonedDateTime.now().minusHours(1), ZonedDateTime.now().minusMinutes(10), false)
		);
		new CertificateStubsSaxWriter(stubs, monolithicSessions).write();
		assertTrue(monolithicSessions.exists());

		// Initialize handler with auto-migrate
		PerElementXmlPersistenceHandler handler = new PerElementXmlPersistenceHandler();
		handler.initialize(Map.of(PARAM_BASE_PATH, testBasePath, PARAM_MODEL_DIR, "model", PARAM_STATE_DIR,
				"state", PARAM_AUTO_MIGRATE_MONOLITHIC, "true", PARAM_PERSIST_SESSIONS, "true"));

		// Verify sessions migrated to individual state files
		File migratedSessionFile = new File(testBasePath, "state/sessions/migrated-session-123.properties");
		assertTrue("Migrated session file should exist at " + migratedSessionFile.getAbsolutePath(),
				migratedSessionFile.exists());

		List<CertificateStub> allSessions = handler.getAllSessions();
		assertEquals(1, allSessions.size());
		CertificateStub migratedStub = allSessions.get(0);
		assertEquals("migrated-session-123", migratedStub.getSessionId());
		assertEquals("admin", migratedStub.getUsername());
		assertEquals("token-abc-123", migratedStub.getAuthToken());

		// Clean up
		removeConfigs(testTarget);
	}

	@Test
	public void shouldHandleCorruptedSessionFileGracefully() throws Exception {
		PerElementXmlPersistenceHandler handler = new PerElementXmlPersistenceHandler();
		handler.initialize(Map.of(PARAM_BASE_PATH, BASE_PATH, PARAM_MODEL_DIR, "model", PARAM_STATE_DIR,
				"state", PARAM_AUTO_MIGRATE_MONOLITHIC, "false", PARAM_PERSIST_SESSIONS, "true"));

		File corruptedFile = new File(BASE_PATH, "state/sessions/corrupted-session.properties");
		Files.writeString(corruptedFile.toPath(), "invalid session properties without required fields\n");

		List<CertificateStub> sessions = handler.getAllSessions();
		assertNotNull(sessions);
		// Corrupted session was ignored
		assertTrue(sessions.stream().noneMatch(s -> "corrupted-session".equals(s.getSessionId())));

		// Clean up
		if (corruptedFile.exists())
			corruptedFile.delete();
	}

	@Test
	public void shouldNotPersistSessionsWhenPersistSessionsFalse() {
		PerElementXmlPersistenceHandler handler = new PerElementXmlPersistenceHandler();
		handler.initialize(Map.of(PARAM_BASE_PATH, BASE_PATH, PARAM_MODEL_DIR, "model", PARAM_STATE_DIR,
				"state", PARAM_AUTO_MIGRATE_MONOLITHIC, "false", PARAM_PERSIST_SESSIONS, "false"));

		Certificate cert = new Certificate(Usage.ANY, "disabled-session-123", "admin", "admin", "First", "Last",
				UserState.ENABLED, "token-123", "127.0.0.1", ZonedDateTime.now(), false, Locale.ENGLISH, Set.of(),
				Set.of(), Map.of());

		handler.addSession(cert);

		File sessionFile = new File(BASE_PATH, "state/sessions/disabled-session-123.properties");
		assertFalse("Session file should NOT be created when persistSessions is false", sessionFile.exists());
		assertTrue("getAllSessions should return empty list when persistSessions is false",
				handler.getAllSessions().isEmpty());
	}

	@Test
	public void shouldNotAutoMigrateSessionsWhenPersistSessionsFalse() throws Exception {
		String testTarget = "PerElementSessionMigrationDisabledTest";
		removeConfigs(testTarget);
		prepareConfigs(testTarget, "PrivilegeConfigPerElement.xml", "PrivilegeUsers.xml", "PrivilegeGroups.xml",
				"PrivilegeRoles.xml");
		String testBasePath = "target/" + testTarget;

		// Write a monolithic PrivilegeSessions.xml in base path
		File monolithicSessions = new File(testBasePath, "PrivilegeSessions.xml");
		List<CertificateStub> stubs = List.of(
				new CertificateStub(Usage.ANY, "migrated-session-disabled", "admin", "token-abc-456", "127.0.0.1",
						Locale.ENGLISH, ZonedDateTime.now().minusHours(1), ZonedDateTime.now().minusMinutes(10), false)
		);
		new CertificateStubsSaxWriter(stubs, monolithicSessions).write();
		assertTrue(monolithicSessions.exists());

		// Initialize handler with auto-migrate but persistSessions=false
		PerElementXmlPersistenceHandler handler = new PerElementXmlPersistenceHandler();
		handler.initialize(Map.of(PARAM_BASE_PATH, testBasePath, PARAM_MODEL_DIR, "model", PARAM_STATE_DIR,
				"state", PARAM_AUTO_MIGRATE_MONOLITHIC, "true", PARAM_PERSIST_SESSIONS, "false"));

		// Verify sessions are NOT migrated to individual state files
		File migratedSessionFile = new File(testBasePath, "state/sessions/migrated-session-disabled.properties");
		assertFalse("Migrated session file should NOT exist when persistSessions is false",
				migratedSessionFile.exists());
		assertTrue(handler.getAllSessions().isEmpty());

		// Clean up
		removeConfigs(testTarget);
	}
}
