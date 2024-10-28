/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResultState;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeAddUserService extends AbstractService<PrivilegeUserArgument, PrivilegeUserResult> {

	@Override
	protected PrivilegeUserResult getResultInstance() {
		return new PrivilegeUserResult(ServiceResultState.FAILED);
	}

	@Override
	public PrivilegeUserArgument getArgumentInstance() {
		return new PrivilegeUserArgument();
	}

	@Override
	protected PrivilegeUserResult internalDoService(PrivilegeUserArgument arg) {

		PrivilegeAddUserCommand cmd;
		try (StrolchTransaction tx = openArgOrUserTx(arg, PrivilegeHandler.PRIVILEGE_ADD_USER)) {
			cmd = new PrivilegeAddUserCommand(tx);
			cmd.setUserIn(arg.user);
			tx.addCommand(cmd);
			tx.commitOnClose();
		}

		return new PrivilegeUserResult(cmd.getUserOut());
	}
}
