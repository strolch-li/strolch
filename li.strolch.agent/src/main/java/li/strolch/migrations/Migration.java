package li.strolch.migrations;

import java.io.File;

import li.strolch.agent.api.ComponentContainer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.Version;

public abstract class Migration {

	protected static final Logger logger = LoggerFactory.getLogger(CodeMigration.class);

	protected final String realm;
	protected final Version version;
	protected final File dataFile;

	public Migration(String realm, Version version, File dataFile) {
		this.realm = realm;
		this.version = version;
		this.dataFile = dataFile;
	}

	public String getRealm() {
		return realm;
	}

	public Version getVersion() {
		return version;
	}

	public File getDataFile() {
		return dataFile;
	}

	public abstract void migrate(ComponentContainer container, Certificate certificate);
}
