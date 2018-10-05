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
package li.strolch.service.privilege.users;

import static org.junit.Assert.assertNotNull;

import java.util.*;

import li.strolch.privilege.model.UserRep;
import li.strolch.privilege.model.UserState;
import li.strolch.service.api.Service;
import li.strolch.service.test.AbstractRealmServiceTest;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeAddUserServiceTest extends AbstractRealmServiceTest<PrivilegeUserArgument, PrivilegeUserResult> {

	@Override
	protected String getUsername() {
		return "admin";
	}

	@Override
	protected Class<? extends Service<PrivilegeUserArgument, PrivilegeUserResult>> getSvcClass() {
		return PrivilegeAddUserService.class;
	}

	@Override
	protected PrivilegeUserArgument getArgInstance() {
		Set<String> roles = new HashSet<>();
		roles.add("AppUser");
		Map<String, String> propertyMap = new HashMap<>();

		PrivilegeUserArgument arg = new PrivilegeUserArgument();
		arg.user = new UserRep(null, "dude", "Jeff", "Lebowski", UserState.ENABLED, roles, Locale.getDefault(),
				propertyMap);

		return arg;
	}

	@Test
	public void runTest() throws IllegalAccessException, InstantiationException {

		runTransient(PrivilegeUserResult.class);

		UserRep dude = runtimeMock.getPrivilegeHandler().getPrivilegeHandler().getUser(certificate, "dude");
		assertNotNull(dude);
	}
}
