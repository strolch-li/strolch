package li.strolch.service.privilege.users;

import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.utils.dbc.DBC;

public class PrivilegeAddUserAndSetPasswordCommand extends PrivilegeAddUserCommand {

	private char[] password;

	public PrivilegeAddUserAndSetPasswordCommand(StrolchTransaction tx) {
		super(tx);
	}

	public void setPassword(char[] password) {
		this.password = password;
	}

	@Override
	public void validate() {
		super.validate();
		DBC.PRE.assertNotNull("password may not be null!", this.password);
	}

	@Override
	public void doCommand() {
		PrivilegeHandler privilegeHandler = getContainer().getPrivilegeHandler().getPrivilegeHandler();
		this.userOut = privilegeHandler.addUser(this.cert, this.userIn, this.password);
		privilegeHandler.persist(this.cert);
		writeAudit();
	}
}
