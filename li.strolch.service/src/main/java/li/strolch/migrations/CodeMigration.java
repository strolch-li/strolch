package li.strolch.migrations;

import java.io.File;

import li.strolch.agent.api.ComponentContainer;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.Version;

public class CodeMigration extends Migration {

	public CodeMigration(String realm, Version version, File dataFile) {
		super(realm, version, dataFile);
	}

	public CodeMigration(String realm, Version version) {
		super(realm, version, null);
	}

	@Override
	public void migrate(ComponentContainer container, Certificate certificate) {
		logger.info("[" + this.realm + "] Running no-op migration " + this.version);
	}
}
