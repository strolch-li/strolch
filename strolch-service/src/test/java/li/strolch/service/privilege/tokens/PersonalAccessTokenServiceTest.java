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
package li.strolch.service.privilege.tokens;

import li.strolch.privilege.model.CreatePersonalAccessTokenArgument;
import li.strolch.privilege.model.PersonalAccessTokenRep;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.privilege.users.PrivilegeUserIdArgument;
import li.strolch.service.test.AbstractRealmServiceTest;
import org.junit.Test;

import java.time.ZonedDateTime;
import java.util.List;

import static org.junit.Assert.*;

public class PersonalAccessTokenServiceTest extends AbstractRealmServiceTest<ServiceArgument, PrivilegeTokenResult> {

	@Override
	protected String getUsername() {
		return "admin";
	}

	@Override
	protected Class getSvcClass() {
		return GetPersonalAccessTokensService.class;
	}

	@Override
	protected ServiceArgument getArgInstance() {
		return new ServiceArgument();
	}

	@Test
	public void shouldManagePersonalAccessTokens() throws Exception {

		// 1. Get tokens (should be empty initially)
		ServiceArgument getArg = getArgInstance();
		getArg.realm = REALM_TRANSIENT;
		System.out.println("[DEBUG_LOG] Admin certificate: " + certificate);
		System.out.println("[DEBUG_LOG] Admin roles: " + certificate.getUserRoles());
		PrivilegeTokenResult result = getServiceHandler().doService(certificate, new GetPersonalAccessTokensService(), getArg);
		assertTrue(result.getMessage(), result.isOk());
		List<PersonalAccessTokenRep> tokens = result.getTokens();
		assertTrue(tokens.isEmpty());

		// 2. Create a token
		CreatePersonalAccessTokenService createSvc = new CreatePersonalAccessTokenService();
		CreatePersonalAccessTokenService.CreatePersonalAccessTokenServiceArgument createArg = new CreatePersonalAccessTokenService.CreatePersonalAccessTokenServiceArgument();
		createArg.realm = REALM_TRANSIENT;
		createArg.arg = new CreatePersonalAccessTokenArgument();
		createArg.arg.name = "Test Token";
		createArg.arg.validFrom = ZonedDateTime.now();
		createArg.arg.validTo = ZonedDateTime.now().plusDays(1);

		PrivilegeTokenResult createResult = getServiceHandler().doService(certificate, createSvc, createArg);
		assertTrue(createResult.getMessage(), createResult.isOk());
		String rawToken = createResult.getRawToken();
		assertNotNull(rawToken);
		assertTrue(rawToken.contains(":"));

		// 3. Get tokens again (should have 1 token)
		result = getServiceHandler().doService(certificate, new GetPersonalAccessTokensService(), getArg);
		assertTrue(result.getMessage(), result.isOk());
		tokens = result.getTokens();
		assertEquals(1, tokens.size());
		assertEquals("Test Token", tokens.get(0).name());
		String tokenId = tokens.get(0).tokenId();

		// 4. Remove the token
		RemovePersonalAccessTokenService removeSvc = new RemovePersonalAccessTokenService();
		PrivilegeUserIdArgument removeArg = new PrivilegeUserIdArgument();
		removeArg.realm = REALM_TRANSIENT;
		removeArg.userId = tokenId; // Note: In this service userId is used for tokenId

		ServiceResult removeResult = getServiceHandler().doService(certificate, removeSvc, removeArg);
		assertTrue(removeResult.getMessage(), removeResult.isOk());

		// 5. Get tokens again (should be empty)
		result = getServiceHandler().doService(certificate, new GetPersonalAccessTokensService(), getArg);
		assertTrue(result.getMessage(), result.isOk());
		tokens = result.getTokens();
		assertTrue(tokens.isEmpty());
	}
}
