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

import static li.strolch.privilege.handler.PrivilegeHandler.PRIVILEGE_SET_USER_PASSWORD;
import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.PRIVILEGE;
import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.USER;

import li.strolch.model.Tags;
import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.service.StringMapArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

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
	protected ServiceResult internalDoService(StringMapArgument arg) throws Exception {

		String username = arg.map.get(Tags.Json.USERNAME);
		String state = arg.map.get(Tags.Json.STATE);

		if (!state.equals("RequirePasswordChange"))
			return ServiceResult.error("Unhandled state " + state);

		try (StrolchTransaction tx = openArgOrUserTx(arg, PRIVILEGE_SET_USER_PASSWORD)) {
			tx.setSuppressAudits(true);

			li.strolch.runtime.privilege.PrivilegeHandler strolchPrivilegeHandler = getContainer().getPrivilegeHandler();
			PrivilegeHandler privilegeHandler = strolchPrivilegeHandler.getPrivilegeHandler();
			privilegeHandler.requirePasswordChange(getCertificate(), username);
			if (privilegeHandler.isPersistOnUserDataChanged())
				privilegeHandler.persist(getCertificate());

			Audit audit = tx.auditFrom(AccessType.UPDATE, PRIVILEGE, USER, username);
			tx.getAuditTrail().add(tx, audit);
		}

		return ServiceResult.success();
	}
}
