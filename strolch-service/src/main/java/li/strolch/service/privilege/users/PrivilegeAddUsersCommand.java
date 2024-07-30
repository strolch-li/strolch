package li.strolch.service.privilege.users;

import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.PRIVILEGE;
import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.USER;

import java.util.List;

import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.UserRep;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

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
		tx().setSuppressAuditsForAudits(true);
		for (UserRep userRep : usersIn) {
			Audit audit = tx().auditFrom(AccessType.CREATE, PRIVILEGE, USER, userRep.getUsername());
			tx().getAuditTrail().add(tx(), audit);
		}
	}
}
