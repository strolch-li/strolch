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

import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants;
import li.strolch.service.api.AbstractService;
import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.model.UserRep;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeSetUserStateService extends AbstractService<PrivilegeSetUserStateArgument, PrivilegeUserResult> {

	private static final long serialVersionUID = 1L;

	@Override
	protected PrivilegeUserResult getResultInstance() {
		return new PrivilegeUserResult();
	}

	@Override
	protected PrivilegeUserResult internalDoService(PrivilegeSetUserStateArgument arg) throws Exception {

		li.strolch.runtime.privilege.PrivilegeHandler strolchPrivilegeHandler = getContainer().getPrivilegeHandler();
		PrivilegeHandler privilegeHandler = strolchPrivilegeHandler.getPrivilegeHandler(getCertificate());

		UserRep user = privilegeHandler.setUserState(getCertificate(), arg.username, arg.userState);

		try (StrolchTransaction tx = openUserTx(PrivilegeHandler.PRIVILEGE_MODIFY_USER)) {
			tx.setSuppressAudits(true);
			Audit audit = tx.auditFrom(AccessType.UPDATE, StrolchPrivilegeConstants.PRIVILEGE,
					StrolchPrivilegeConstants.USER, user.getUsername());
			tx.getAuditTrail().add(tx, audit);
		}

		return new PrivilegeUserResult(user);
	}

	@Override
	public String getPrivilegeName() {
		return StrolchPrivilegeConstants.PRIVILEGE_MODIFY_USER;
	}

	@Override
	public Object getPrivilegeValue() {
		return null;
	}
}
