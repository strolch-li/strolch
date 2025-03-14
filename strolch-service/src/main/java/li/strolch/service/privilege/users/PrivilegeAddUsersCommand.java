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
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.UserRep;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

import java.util.List;

import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.PRIVILEGE;
import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.USER;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeAddUsersCommand extends Command {

	// input
	protected List<UserRep> usersIn;
	protected Certificate cert;

	public PrivilegeAddUsersCommand(StrolchTransaction tx) {
		super(tx);
	}

	public void setUsersIn(List<UserRep> usersIn) {
		this.usersIn = usersIn;
	}

	public void setCert(Certificate cert) {
		this.cert = cert;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotEmpty("usersIn may not be empty!", this.usersIn);
		if (this.cert == null)
			this.cert = tx().getCertificate();
	}

	@Override
	public void doCommand() {
		PrivilegeHandler privilegeHandler = getContainer().getPrivilegeHandler().getPrivilegeHandler();
		privilegeHandler.addOrUpdateUsers(this.cert, this.usersIn);
		if (privilegeHandler.isPersistOnUserDataChanged())
			privilegeHandler.persist(this.cert);
		writeAudits();
	}

	protected void writeAudits() {
		StrolchTransaction tx = tx();
		for (UserRep userRep : usersIn) {
			tx.add(tx.auditFrom(AccessType.CREATE, PRIVILEGE, USER, userRep.getUsername()));
		}
	}
}
