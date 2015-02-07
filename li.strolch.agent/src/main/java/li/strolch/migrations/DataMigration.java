package li.strolch.migrations;

import java.io.File;

import li.strolch.agent.api.ComponentContainer;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.Version;

public class DataMigration extends Migration {

	public DataMigration(String realm, Version version, File dataFile) {
		super(realm, version, dataFile);
	}

	@Override
	public void migrate(ComponentContainer container, Certificate certificate) {

		logger.info("[" + this.realm + "] Running migration " + this.version);
	}
}
