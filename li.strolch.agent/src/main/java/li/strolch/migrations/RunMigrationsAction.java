package li.strolch.migrations;

import ch.eitchnet.privilege.handler.SystemUserAction;
import ch.eitchnet.privilege.model.PrivilegeContext;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RunMigrationsAction implements SystemUserAction {

	private Migrations migrations;

	/**
	 * @param migrations
	 */
	public RunMigrationsAction(Migrations migrations) {
		this.migrations = migrations;
	}

	@Override
	public void execute(PrivilegeContext privilegeContext) {
		this.migrations.runMigrations(privilegeContext.getCertificate());
	}
}
