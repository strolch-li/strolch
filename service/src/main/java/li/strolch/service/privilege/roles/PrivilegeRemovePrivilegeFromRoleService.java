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
package li.strolch.service.privilege.roles;

import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.model.RoleRep;
import li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResultState;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeRemovePrivilegeFromRoleService
		extends AbstractService<PrivilegeRemovePrivilegeFromRoleArgument, PrivilegeRoleResult> {

	@Override
	protected PrivilegeRoleResult getResultInstance() {
		return new PrivilegeRoleResult(ServiceResultState.FAILED);
	}

	@Override
	public PrivilegeRemovePrivilegeFromRoleArgument getArgumentInstance() {
		return new PrivilegeRemovePrivilegeFromRoleArgument();
	}

	@Override
	protected PrivilegeRoleResult internalDoService(PrivilegeRemovePrivilegeFromRoleArgument arg) throws Exception {

		li.strolch.runtime.privilege.PrivilegeHandler strolchPrivilegeHandler = getContainer().getPrivilegeHandler();
		PrivilegeHandler privilegeHandler = strolchPrivilegeHandler.getPrivilegeHandler();

		RoleRep role;
		try (StrolchTransaction tx = openArgOrUserTx(arg, StrolchPrivilegeConstants.PRIVILEGE_MODIFY_ROLE)) {
			tx.setSuppressAudits(true);

			role = privilegeHandler.removePrivilegeFromRole(getCertificate(), arg.roleName, arg.privilegeName);
			privilegeHandler.persist(getCertificate());

			Audit audit = tx
					.auditFrom(AccessType.UPDATE, StrolchPrivilegeConstants.PRIVILEGE, StrolchPrivilegeConstants.ROLE,
							role.getName());
			tx.getAuditTrail().add(tx, audit);
		}

		return new PrivilegeRoleResult(role);
	}
}
