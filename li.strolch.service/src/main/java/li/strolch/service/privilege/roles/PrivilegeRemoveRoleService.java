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

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeRemoveRoleService extends AbstractService<PrivilegeRoleNameArgument, PrivilegeRoleResult> {

	private static final long serialVersionUID = 1L;

	@Override
	protected PrivilegeRoleResult getResultInstance() {
		return new PrivilegeRoleResult();
	}

	@Override
	protected PrivilegeRoleResult internalDoService(PrivilegeRoleNameArgument arg) throws Exception {

		li.strolch.runtime.privilege.PrivilegeHandler strolchPrivilegeHandler = getContainer().getPrivilegeHandler();
		PrivilegeHandler privilegeHandler = strolchPrivilegeHandler.getPrivilegeHandler(getCertificate());

		RoleRep role = privilegeHandler.removeRole(getCertificate(), arg.roleName);

		try (StrolchTransaction tx = openUserTx(PrivilegeHandler.PRIVILEGE_REMOVE_ROLE)) {
			tx.setSuppressAudits(true);
			Audit audit = tx.auditFrom(AccessType.DELETE, StrolchPrivilegeConstants.PRIVILEGE,
					StrolchPrivilegeConstants.ROLE, role.getName());
			tx.getAuditTrail().add(tx, audit);
		}

		return new PrivilegeRoleResult(role);
	}

	@Override
	public String getPrivilegeName() {
		return StrolchPrivilegeConstants.PRIVILEGE_REMOVE_ROLE;
	}

	@Override
	public Object getPrivilegeValue() {
		return null;
	}
}
