/*
 * Copyright (c) 2015-2025 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.model.CreatePersonalAccessTokenArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceResultState;

import static li.strolch.privilege.handler.PrivilegeHandler.PRIVILEGE_CREATE_PERSONAL_ACCESS_TOKEN;

public class CreatePersonalAccessTokenService extends AbstractService<CreatePersonalAccessTokenService.CreatePersonalAccessTokenServiceArgument, PrivilegeTokenResult> {

	@Override
	protected PrivilegeTokenResult getResultInstance() {
		return new PrivilegeTokenResult(ServiceResultState.FAILED);
	}

	@Override
	public CreatePersonalAccessTokenServiceArgument getArgumentInstance() {
		return new CreatePersonalAccessTokenServiceArgument();
	}

	@Override
	protected PrivilegeTokenResult internalDoService(CreatePersonalAccessTokenServiceArgument arg) {
		PrivilegeHandler privilegeHandler = getContainer().getPrivilegeHandler().getPrivilegeHandler();

		String rawToken;
		try (StrolchTransaction tx = openArgOrUserTx(arg, PRIVILEGE_CREATE_PERSONAL_ACCESS_TOKEN)) {
			rawToken = privilegeHandler.createPersonalAccessToken(tx.getCertificate(), arg.arg.name, arg.arg.validFrom,
					arg.arg.validTo, arg.arg.roles, arg.arg.privileges);
			tx.commitOnClose();
		}

		return new PrivilegeTokenResult(rawToken);
	}

	public static class CreatePersonalAccessTokenServiceArgument extends ServiceArgument {
		public CreatePersonalAccessTokenArgument arg;
	}
}
