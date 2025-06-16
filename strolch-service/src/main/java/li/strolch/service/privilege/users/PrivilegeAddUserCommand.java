/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.UserRep;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.PRIVILEGE;
import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.USER;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeAddUserCommand extends Command {

	// input
	protected UserRep userIn;
	protected Certificate cert;

	// intermediary
	protected Audit audit;

	// output
	protected UserRep userOut;

	public PrivilegeAddUserCommand(StrolchTransaction tx) {
		super(tx);
	}

	public void setUserIn(UserRep userIn) {
		this.userIn = userIn;
	}

	public void setCert(Certificate cert) {
		this.cert = cert;
	}

	public UserRep getUserOut() {
		return this.userOut;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("userIn may not be null!", this.userIn);
		if (this.cert == null)
			this.cert = tx().getCertificate();
	}

	@Override
	public void doCommand() {
		PrivilegeHandler privilegeHandler = getContainer().getPrivilegeHandler().getPrivilegeHandler();
		this.userOut = privilegeHandler.addUser(this.cert, this.userIn, null);
		if (privilegeHandler.isPersistOnUserDataChanged())
			privilegeHandler.persist(this.cert);
		writeAudit();
	}

	protected void writeAudit() {
		this.audit = tx().auditFrom(AccessType.CREATE, PRIVILEGE, USER, this.userOut.getUsername());
		tx().add(this.audit);
	}

	@Override
	public void undo() {
		if (tx().isRollingBack()) {
			PrivilegeHandler privilegeHandler = getContainer().getPrivilegeHandler().getPrivilegeHandler();

			if (this.userOut != null)
				privilegeHandler.removeUser(tx().getCertificate(), this.userIn.getUsername());
		}
	}
}
