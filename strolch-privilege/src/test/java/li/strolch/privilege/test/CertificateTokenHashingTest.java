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

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.NotAuthenticatedException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Usage;
import li.strolch.privilege.model.internal.PasswordCrypt;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.File;
import java.nio.file.Files;

import static li.strolch.privilege.helper.XmlConstants.PARAM_SESSIONS_FILE_DEF;
import static org.junit.Assert.*;

public class CertificateTokenHashingTest extends AbstractPrivilegeTest {

	private static final String TARGET = CertificateTokenHashingTest.class.getSimpleName();

	@BeforeClass
	public static void beforeClass() {
		AbstractPrivilegeTest.beforeClass();
		prepareConfigs(TARGET, "PrivilegeConfig.xml", "PrivilegeUsers.xml", "PrivilegeGroups.xml",
				"PrivilegeRoles.xml");
	}

	@AfterClass
	public static void afterClass() {
		removeConfigs(TARGET);
		AbstractPrivilegeTest.afterClass();
	}

	@Before
	public void before() {
		initialize(TARGET, "PrivilegeConfig.xml");
	}

	@Test
	public void shouldAuthenticateAndGenerateHashedToken() {
		Certificate cert = this.privilegeHandler.authenticate("admin", "admin".toCharArray(), "test-source",
				Usage.ANY, false);
		assertNotNull(cert);
		assertNotNull(cert.getAuthToken());
		assertTrue("AuthToken should be sessionId:tokenValue", cert.getAuthToken().contains(":"));

		PasswordCrypt crypt = cert.getAuthTokenCrypt();
		assertNotNull(crypt);
		assertNotNull(crypt.password());
		assertNotNull(crypt.salt());
		assertNotNull(crypt.buildPasswordString());
		assertTrue("Password string should start with $ algorithm", crypt.buildPasswordString().startsWith("$"));

		String tokenValue = cert.getAuthToken().substring(cert.getAuthToken().indexOf(':') + 1);
		assertFalse("Crypt should not contain plaintext token", crypt.buildPasswordString().contains(tokenValue));
	}

	@Test
	public void shouldValidateViaFastHashCache() {
		Certificate cert = this.privilegeHandler.authenticate("admin", "admin".toCharArray(), "test-source",
				Usage.ANY, false);
		String authToken = cert.getAuthToken();

		// Warm-up and consecutive fast validations
		for (int i = 0; i < 50; i++) {
			long start = System.nanoTime();
			PrivilegeContext validatedCtx = this.privilegeHandler.validate(authToken, "test-source");
			long durationNanos = System.nanoTime() - start;
			assertNotNull(validatedCtx);
			assertEquals(cert.getSessionId(), validatedCtx.getCertificate().getSessionId());
			// Validation via SHA-256 fast cache should easily execute in sub-millisecond time (< 500us)
			assertTrue("Validation took too long: " + durationNanos + " ns", durationNanos < 10_000_000);
		}
	}

	@Test
	public void shouldPersistHashedTokenAndReloadColdCache() throws Exception {
		Certificate cert = this.privilegeHandler.authenticate("admin", "admin".toCharArray(), "test-source",
				Usage.ANY, false);
		String authToken = cert.getAuthToken();
		String sessionId = cert.getSessionId();
		String tokenValue = authToken.substring(authToken.indexOf(':') + 1);

		// Allow async session persistence to write to disk
		Thread.sleep(1200L);

		File targetPath = new File("target", TARGET);
		File sessionsFile = new File(targetPath, PARAM_SESSIONS_FILE_DEF);
		assertTrue("Sessions file should exist", sessionsFile.isFile());

		String fileContent = Files.readString(sessionsFile.toPath());
		assertFalse("Persisted file must NOT contain plaintext token value", fileContent.contains(tokenValue));
		assertTrue("Persisted file must contain PasswordCrypt string", fileContent.contains("$PBKDF2WithHmacSHA512"));

		// Simulate server restart by re-initializing PrivilegeHandler
		initialize(TARGET, "PrivilegeConfig.xml");

		// Cold-cache request with the client's authToken
		long start = System.nanoTime();
		PrivilegeContext reloadedCtx = this.privilegeHandler.validate(authToken, "test-source");
		long coldDurationNanos = System.nanoTime() - start;
		assertNotNull(reloadedCtx);
		assertEquals(sessionId, reloadedCtx.getCertificate().getSessionId());

		// Subsequent request should hit fast-hash cache
		start = System.nanoTime();
		PrivilegeContext cachedCtx = this.privilegeHandler.validate(authToken, "test-source");
		long cachedDurationNanos = System.nanoTime() - start;
		assertNotNull(cachedCtx);
		assertTrue(cachedDurationNanos <= coldDurationNanos);
	}

	@Test
	public void shouldRejectInvalidAndTamperedTokens() {
		Certificate cert = this.privilegeHandler.authenticate("admin", "admin".toCharArray(), "test-source",
				Usage.ANY, false);
		String authToken = cert.getAuthToken();
		String sessionId = cert.getSessionId();

		// Unknown sessionId
		assertThrows(NotAuthenticatedException.class, () ->
				this.privilegeHandler.validate("unknown-session-id:token", "test-source"));

		// Tampered token secret
		assertThrows(AccessDeniedException.class, () ->
				this.privilegeHandler.validate(sessionId + ":wrongSecretToken", "test-source"));

		// Empty token
		assertThrows(NotAuthenticatedException.class, () ->
				this.privilegeHandler.validate("", "test-source"));
	}

	@Test
	public void shouldInvalidateSessionAndPurgeCache() throws Exception {
		Certificate cert = this.privilegeHandler.authenticate("admin", "admin".toCharArray(), "test-source",
				Usage.ANY, false);
		String authToken = cert.getAuthToken();

		PrivilegeContext ctx = this.privilegeHandler.validate(authToken, "test-source");
		assertNotNull(ctx);

		boolean invalidated = this.privilegeHandler.invalidate(cert);
		assertTrue(invalidated);

		// Subsequent validation must fail
		assertThrows(NotAuthenticatedException.class, () ->
				this.privilegeHandler.validate(authToken, "test-source"));
	}
}
