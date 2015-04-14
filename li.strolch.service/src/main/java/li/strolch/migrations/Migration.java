package li.strolch.migrations;

import java.io.File;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.command.AddResourceCommand;
import li.strolch.command.parameter.SetParameterCommand;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.Version;

public abstract class Migration {

	public static final String MIGRATIONS_TYPE = "Migrations";
	public static final String MIGRATIONS_ID = "migrations";
	public static final String BAG_PARAMETERS = "parameters";
	public static final String PARAM_CURRENT_VERSION = "currentVersion";

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

	protected StrolchTransaction openTx(ComponentContainer container, Certificate cert) {
		return container.getRealm(getRealm()).openTx(cert, getClass());
	}

	protected void buildMigrationVersionChangeCommand(ComponentContainer container, StrolchTransaction tx) {

		Resource migrationsRes = tx.getResourceBy(MIGRATIONS_TYPE, MIGRATIONS_ID);
		if (migrationsRes == null) {
			migrationsRes = new Resource(MIGRATIONS_ID, MIGRATIONS_TYPE, MIGRATIONS_TYPE);

			ParameterBag bag = new ParameterBag(BAG_PARAMETERS, BAG_PARAMETERS, BAG_PARAMETERS);
			migrationsRes.addParameterBag(bag);

			StringParameter currentVersionP = new StringParameter(PARAM_CURRENT_VERSION, PARAM_CURRENT_VERSION,
					getVersion().toString());
			bag.addParameter(currentVersionP);

			AddResourceCommand cmd = new AddResourceCommand(container, tx);
			cmd.setResource(migrationsRes);

			tx.addCommand(cmd);

		} else {

			StringParameter currentVersionP = migrationsRes.getParameter(BAG_PARAMETERS, PARAM_CURRENT_VERSION);

			SetParameterCommand cmd = new SetParameterCommand(container, tx);
			cmd.setParameter(currentVersionP);
			cmd.setValueAsString(getVersion().toString());

			tx.addCommand(cmd);
		}
	}

	public abstract void migrate(ComponentContainer container, Certificate certificate);
}
