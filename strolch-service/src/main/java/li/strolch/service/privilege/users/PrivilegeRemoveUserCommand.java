/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.model.UserRep;
import li.strolch.runtime.sessions.StrolchSessionHandler;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.PRIVILEGE;
import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.USER;

public class PrivilegeRemoveUserCommand extends Command {

	private String userId;

	public PrivilegeRemoveUserCommand(StrolchTransaction tx) {
		super(tx);
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotEmpty("userId must be set", this.userId);
	}

	@Override
	public void doCommand() {

		li.strolch.runtime.privilege.PrivilegeHandler strolchPrivilegeHandler = getContainer().getPrivilegeHandler();
		PrivilegeHandler privilegeHandler = strolchPrivilegeHandler.getPrivilegeHandler();

		UserRep userRep = privilegeHandler.removeUserById(tx().getCertificate(), this.userId);
		if (privilegeHandler.isPersistOnUserDataChanged())
			privilegeHandler.persist(tx().getCertificate());

		getComponent(StrolchSessionHandler.class).refreshSessions();

		tx().add(tx().auditFrom(AccessType.DELETE, PRIVILEGE, USER, userRep.getUsername()));
	}
}
