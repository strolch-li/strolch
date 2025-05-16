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
package li.strolch.service.privilege.users;

import li.strolch.model.Tags;
import li.strolch.model.audit.AccessType;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.model.UserRep;
import li.strolch.service.StringMapArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

import static li.strolch.privilege.handler.PrivilegeHandler.PRIVILEGE_SET_USER_PASSWORD;
import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.PRIVILEGE;
import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.USER;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeSetUserPasswordStateService extends AbstractService<StringMapArgument, ServiceResult> {

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult(ServiceResultState.FAILED);
	}

	@Override
	public StringMapArgument getArgumentInstance() {
		return new StringMapArgument();
	}

	@Override
	protected ServiceResult internalDoService(StringMapArgument arg) {

		String userId = arg.map.get(Tags.Json.USER_ID);
		String state = arg.map.get(Tags.Json.STATE);

		if (!state.equals("RequirePasswordChange"))
			return ServiceResult.error("Unhandled state " + state);

		try (StrolchTransaction tx = openArgOrUserTx(arg, PRIVILEGE_SET_USER_PASSWORD)) {
			li.strolch.runtime.privilege.PrivilegeHandler strolchPrivilegeHandler
					= getContainer().getPrivilegeHandler();
			PrivilegeHandler privilegeHandler = strolchPrivilegeHandler.getPrivilegeHandler();
			UserRep userRep = privilegeHandler.requirePasswordChangeById(getCertificate(), userId);
			if (privilegeHandler.isPersistOnUserDataChanged())
				privilegeHandler.persist(getCertificate());

			tx.add(tx.auditFrom(AccessType.UPDATE, PRIVILEGE, USER, userRep.getUsername()));
			tx.commitOnClose();
		}

		return ServiceResult.success();
	}
}
