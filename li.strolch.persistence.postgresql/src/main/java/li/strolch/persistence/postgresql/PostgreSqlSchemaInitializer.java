/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.persistence.postgresql;

import java.util.Map;
import java.util.Map.Entry;

import li.strolch.agent.api.StrolchAgent;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeContext;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PostgreSqlSchemaInitializer extends PostgreSqlInitializer {

	private Map<String, DbMigrationState> dbMigrationStates;

	/**
	 * @param agent
	 * @param persistenceHandler
	 * @param dbMigrationStates
	 */
	public PostgreSqlSchemaInitializer(StrolchAgent agent, PostgreSqlPersistenceHandler persistenceHandler,
			Map<String, DbMigrationState> dbMigrationStates) {
		super(agent, persistenceHandler);
		this.dbMigrationStates = dbMigrationStates;
	}

	private Certificate certificate;

	@Override
	public void execute(PrivilegeContext privilegeContext) {
		this.certificate = privilegeContext.getCertificate();

		// first make sure the data store exists if needed
		for (Entry<String, DbMigrationState> entry : this.dbMigrationStates.entrySet()) {
			if (checkNeedsDbInit(entry.getValue()))
				getDataStoreFile(runtimeConfig, realmConfig, entry.getKey());
		}

		// then initialize the schemas
		for (Entry<String, DbMigrationState> entry : this.dbMigrationStates.entrySet()) {
			initSchemaFromDataStore(entry.getValue(), entry.getKey());
		}
	}

	@Override
	protected Certificate getCertificate() {
		return this.certificate;
	}
}
