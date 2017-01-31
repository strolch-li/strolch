package li.strolch.service.privilege.roles;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;

public class PrivilageAddOrReplacePrivilegeOnRoleCommand extends Command {

	public PrivilageAddOrReplacePrivilegeOnRoleCommand(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	public void validate() {
		// TODO Auto-generated method stub

	}

	@Override
	public void doCommand() {
		// TODO Auto-generated method stub

	}

	@Override
	public void undo() {
		// TODO Auto-generated method stub

	}

}
