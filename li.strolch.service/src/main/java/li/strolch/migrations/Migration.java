/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package li.strolch.migrations;

import java.io.File;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.command.parameter.SetParameterCommand;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.AddResourceCommand;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.utils.Version;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class Migration {

	public static final String MIGRATIONS_TYPE = "Migrations";
	public static final String MIGRATIONS_ID = "migrations";
	public static final String BAG_PARAMETERS = "parameters";
	public static final String PARAM_CURRENT_DATA_VERSION = "currentDataVersion";
	public static final String PARAM_CURRENT_CODE_VERSION = "currentCodeVersion";

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

			StringParameter currentDataVersionP = new StringParameter(PARAM_CURRENT_DATA_VERSION,
					PARAM_CURRENT_DATA_VERSION, getVersion().toString());
			bag.addParameter(currentDataVersionP);

			StringParameter currentCodeVersionP = new StringParameter(PARAM_CURRENT_CODE_VERSION,
					PARAM_CURRENT_CODE_VERSION, getVersion().toString());
			bag.addParameter(currentCodeVersionP);

			AddResourceCommand cmd = new AddResourceCommand(container, tx);
			cmd.setResource(migrationsRes);

			tx.addCommand(cmd);

		} else {
			SetParameterCommand cmd = new SetParameterCommand(container, tx);

			setNewVersion(cmd,migrationsRes);

			tx.addCommand(cmd);
		}
	}

	public abstract void migrate(ComponentContainer container, Certificate certificate);
	
	protected abstract void setNewVersion(SetParameterCommand cmd,Resource migrationsRes);
}
