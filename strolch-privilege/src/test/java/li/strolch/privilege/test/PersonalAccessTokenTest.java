/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PersonalAccessTokenRep;
import li.strolch.privilege.model.Privilege;
import li.strolch.privilege.model.PrivilegeContext;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.time.ZonedDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import static org.junit.Assert.*;

public class PersonalAccessTokenTest extends AbstractPrivilegeTest {

	private static final String TARGET_DIR = PersonalAccessTokenTest.class.getSimpleName();

	@Before
	public void setup() {
		prepareConfigs(TARGET_DIR, "PrivilegeConfig.xml", "PrivilegeUsers.xml", "PrivilegeGroups.xml",
				"PrivilegeRoles.xml");
		initialize(TARGET_DIR, "PrivilegeConfig.xml");
	}

	@After
	public void destroy() {
		removeConfigs(TARGET_DIR);
	}

	@Test
	public void shouldManagePersonalAccessTokens() {
		login("admin", "admin".toCharArray());
		Certificate cert = this.ctx.getCertificate();

		ZonedDateTime validFrom = ZonedDateTime.now();
		ZonedDateTime validTo = validFrom.plusYears(1);

		// Create
		String token = this.privilegeHandler.createPersonalAccessToken(cert, "Test Token", validFrom, validTo, null,
				null);
		assertNotNull(token);

		// List
		List<PersonalAccessTokenRep> tokens = this.privilegeHandler.getPersonalAccessTokens(cert);
		assertEquals(1, tokens.size());
		PersonalAccessTokenRep tokenRep = tokens.get(0);
		assertEquals("Test Token", tokenRep.name());
		assertEquals("admin", tokenRep.username());
		assertNull(tokenRep.lastUsed());

		// Authenticate
		Certificate apiCert = this.privilegeHandler.authenticatePersonalAccessToken(token, "api-test");
		assertNotNull(apiCert);
		assertEquals("admin", apiCert.getUsername());
		assertTrue(apiCert.getUsage().isApi());

		// Verify last used updated
		tokens = this.privilegeHandler.getPersonalAccessTokens(cert);
		assertNotNull(tokens.get(0).lastUsed());

		// Remove
		this.privilegeHandler.removePersonalAccessToken(cert, tokenRep.tokenId());
		tokens = this.privilegeHandler.getPersonalAccessTokens(cert);
		assertTrue(tokens.isEmpty());

		// Authenticate should now fail
		try {
			this.privilegeHandler.authenticatePersonalAccessToken(token, "api-test");
			fail("Should have failed to authenticate with removed token");
		} catch (Exception e) {
			assertEquals("Invalid personal access token!", e.getMessage());
		}
	}

	@Test
	public void shouldEnforceTokenExpiration() {
		login("admin", "admin".toCharArray());
		Certificate cert = this.ctx.getCertificate();

		ZonedDateTime validFrom = ZonedDateTime.now().minusDays(2);
		ZonedDateTime validTo = ZonedDateTime.now().minusDays(1);

		String token = this.privilegeHandler.createPersonalAccessToken(cert, "Expired Token", validFrom, validTo,
				null, null);

		try {
			this.privilegeHandler.authenticatePersonalAccessToken(token, "api-test");
			fail("Should have failed to authenticate with expired token");
		} catch (Exception e) {
			assertEquals("Personal access token is expired or not yet valid!", e.getMessage());
		}
	}

	@Test
	public void shouldRestrictPrivilegesToTokenScope() {
		login("admin", "admin".toCharArray());
		Certificate cert = this.ctx.getCertificate();

		String token = this.privilegeHandler.createPersonalAccessToken(cert, "Scope Token", ZonedDateTime.now(),
				ZonedDateTime.now().plusDays(1), null, null);
		Certificate apiCert = this.privilegeHandler.authenticatePersonalAccessToken(token, "api-test");
		PrivilegeContext apiCtx = this.privilegeHandler.validate(apiCert);

		// Admin has many privileges, check one
		apiCtx.assertHasPrivilege("PrivilegeAction");
	}

	@Test
	public void shouldCreateTokenWithRolesSubset() {
		login("admin", "admin".toCharArray());
		Certificate cert = this.ctx.getCertificate();

		// admin has PrivilegeAdmin role
		Set<String> roles = Set.of("PrivilegeAdmin");
		String token = this.privilegeHandler.createPersonalAccessToken(cert, "Roles Subset Token", ZonedDateTime.now(),
				ZonedDateTime.now().plusDays(1), roles, null);

		Certificate apiCert = this.privilegeHandler.authenticatePersonalAccessToken(token, "api-test");
		PrivilegeContext apiCtx = this.privilegeHandler.validate(apiCert);

		// Should have privileges from PrivilegeAdmin
		apiCtx.assertHasPrivilege("PrivilegeAction");
	}

	@Test
	public void shouldCreateTokenWithPrivilegesSubset() {
		login("admin", "admin".toCharArray());
		Certificate cert = this.ctx.getCertificate();

		// Get user's privileges first
		PrivilegeContext adminCtx = this.privilegeHandler.validate(cert);
		Privilege privilegeAction = adminCtx.getPrivileges().get("PrivilegeAction");
		assertNotNull(privilegeAction);

		List<Privilege> privileges = List.of(privilegeAction);
		String token = this.privilegeHandler.createPersonalAccessToken(cert, "Privileges Subset Token",
				ZonedDateTime.now(), ZonedDateTime.now().plusDays(1), null, privileges);

		Certificate apiCert = this.privilegeHandler.authenticatePersonalAccessToken(token, "api-test");
		PrivilegeContext apiCtx = this.privilegeHandler.validate(apiCert);

		// Should have PrivilegeAction
		apiCtx.assertHasPrivilege("PrivilegeAction");
		// Should NOT have other privileges (e.g. PrivilegeGetUser)
		assertFalse(apiCtx.getPrivileges().containsKey("PrivilegeGetUser"));
	}

	@Test
	public void shouldFailToCreateTokenWithoutPrivilege() {
		// jill does not have PrivilegeCreatePersonalAccessToken
		login("jill", "admin".toCharArray());
		Certificate cert = this.ctx.getCertificate();

		try {
			this.privilegeHandler.createPersonalAccessToken(cert, "Jill Token", ZonedDateTime.now(),
					ZonedDateTime.now().plusDays(1), null, null);
			fail("Should have failed to create token without privilege");
		} catch (Exception e) {
			assertTrue(e.getMessage().contains("PrivilegeCreatePersonalAccessToken"));
		}
	}

	@Test
	public void shouldNotEscalatePrivileges() {
		login("admin", "admin".toCharArray());
		Certificate cert = this.ctx.getCertificate();

		// try to create a token with a privilege admin doesn't have
		// we'll define a completely new privilege that NO role has

		Privilege escalationPrivilege = new Privilege("NonExistentPrivilege", "DefaultPrivilege", true,
				Collections.emptySet(), Collections.emptySet());

		try {
			this.privilegeHandler.createPersonalAccessToken(cert, "Escalation Token", ZonedDateTime.now(),
					ZonedDateTime.now().plusDays(1), null, List.of(escalationPrivilege));
			fail("Should have failed to create token with escalation");
		} catch (Exception e) {
			assertTrue(e.getMessage().contains("does not have any of the given roles or privileges"));
		}
	}

	@Test
	public void shouldCachePersonalAccessToken() {
		login("admin", "admin".toCharArray());
		Certificate cert = this.ctx.getCertificate();

		ZonedDateTime validFrom = ZonedDateTime.now();
		ZonedDateTime validTo = validFrom.plusYears(1);

		String token = this.privilegeHandler.createPersonalAccessToken(cert, "Cache Token", validFrom, validTo, null,
				null);
		assertNotNull(token);

		// First authentication - should hash and cache
		Certificate apiCert1 = this.privilegeHandler.authenticatePersonalAccessToken(token, "api-test");
		assertNotNull(apiCert1);

		// Second authentication - should use cache
		Certificate apiCert2 = this.privilegeHandler.authenticatePersonalAccessToken(token, "api-test");
		assertNotNull(apiCert2);
		assertEquals(apiCert1.getSessionId(), apiCert2.getSessionId());

		// Verify revocation clears cache
		String tokenId = token.split(":")[0];
		this.privilegeHandler.removePersonalAccessToken(cert, tokenId);

		try {
			this.privilegeHandler.authenticatePersonalAccessToken(token, "api-test");
			fail("Should have failed to authenticate with revoked token");
		} catch (Exception e) {
			assertEquals("Invalid personal access token!", e.getMessage());
		}
	}
}
