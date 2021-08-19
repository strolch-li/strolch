package li.strolch.service.privilege.users;

import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.PRIVILEGE;
import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.USER;

import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

public class PrivilegeRemoveUserCommand extends Command {

	private String username;

	public PrivilegeRemoveUserCommand(StrolchTransaction tx) {
		super(tx);
	}

	public void setUsername(String username) {
		this.username = username;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotEmpty("username must be set", this.username);
	}

	@Override
	public void doCommand() {

		li.strolch.runtime.privilege.PrivilegeHandler strolchPrivilegeHandler = getContainer().getPrivilegeHandler();
		PrivilegeHandler privilegeHandler = strolchPrivilegeHandler.getPrivilegeHandler();

		privilegeHandler.removeUser(tx().getCertificate(), this.username);
		privilegeHandler.persist(tx().getCertificate());

		Audit audit = tx().auditFrom(AccessType.DELETE, PRIVILEGE, USER, this.username);
		tx().getAuditTrail().add(tx(), audit);
	}
}
