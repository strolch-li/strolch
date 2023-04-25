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

import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.model.UserRep;
import li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants;
import li.strolch.service.api.AbstractService;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeRemoveRoleFromUserService
		extends AbstractService<PrivilegeRoleUserNamesArgument, PrivilegeUserResult> {

	@Override
	protected PrivilegeUserResult getResultInstance() {
		return new PrivilegeUserResult();
	}

	@Override
	public PrivilegeRoleUserNamesArgument getArgumentInstance() {
		return new PrivilegeRoleUserNamesArgument();
	}

	@Override
	protected PrivilegeUserResult internalDoService(PrivilegeRoleUserNamesArgument arg) {

		li.strolch.runtime.privilege.PrivilegeHandler strolchPrivilegeHandler = getContainer().getPrivilegeHandler();
		PrivilegeHandler privilegeHandler = strolchPrivilegeHandler.getPrivilegeHandler();

		UserRep user;
		try (StrolchTransaction tx = openArgOrUserTx(arg, PrivilegeHandler.PRIVILEGE_REMOVE_ROLE_FROM_USER)) {
			tx.setSuppressAudits(true);

			user = privilegeHandler.removeRoleFromUser(getCertificate(), arg.username, arg.rolename);
			if (privilegeHandler.isPersistOnUserDataChanged())
				privilegeHandler.persist(getCertificate());

			Audit audit = tx.auditFrom(AccessType.UPDATE, StrolchPrivilegeConstants.PRIVILEGE,
					StrolchPrivilegeConstants.USER, user.getUsername());
			tx.getAuditTrail().add(tx, audit);
		}

		return new PrivilegeUserResult(user);
	}
}
