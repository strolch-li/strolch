package li.strolch.migrations;

import java.util.Map;

import ch.eitchnet.privilege.handler.SystemUserAction;
import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.utils.Version;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RunMigrationsAction implements SystemUserAction {

	private Migrations migrations;
	private Map<String, Version> currentVersions;

	/**
	 * @param migrations
	 * @param currentVersions
	 */
	public RunMigrationsAction(Migrations migrations, Map<String, Version> currentVersions) {
		this.migrations = migrations;
		this.currentVersions = currentVersions;
	}

	@Override
	public void execute(PrivilegeContext privilegeContext) {
		this.migrations.runMigrations(privilegeContext.getCertificate(), currentVersions);
	}
}
