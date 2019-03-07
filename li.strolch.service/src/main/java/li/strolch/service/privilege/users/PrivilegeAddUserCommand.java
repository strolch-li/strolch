package li.strolch.service.privilege.users;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.UserRep;
import li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeAddUserCommand extends Command {

	// input
	private UserRep userIn;
	private Certificate cert;

	// intermediary
	private Audit audit;

	// output
	private UserRep userOut;

	public PrivilegeAddUserCommand(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
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
		privilegeHandler.persist(this.cert);

		tx().setSuppressAuditsForAudits(true);

		this.audit = tx()
				.auditFrom(AccessType.CREATE, StrolchPrivilegeConstants.PRIVILEGE, StrolchPrivilegeConstants.USER,
						this.userOut.getUsername());
		tx().getAuditTrail().add(tx(), this.audit);
	}

	@Override
	public void undo() {

		if (tx().isRollingBack()) {
			PrivilegeHandler privilegeHandler = getContainer().getPrivilegeHandler().getPrivilegeHandler();

			if (this.userOut != null)
				privilegeHandler.removeUser(tx().getCertificate(), this.userIn.getUsername());

			if (this.audit != null)
				tx().getAuditTrail().remove(tx(), this.audit);
		}
	}
}
