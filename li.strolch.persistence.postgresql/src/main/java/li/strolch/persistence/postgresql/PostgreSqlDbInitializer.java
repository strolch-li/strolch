/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 */
package li.strolch.persistence.postgresql;

import static ch.eitchnet.db.DbConstants.PROP_ALLOW_SCHEMA_CREATION;
import static ch.eitchnet.db.DbConstants.PROP_ALLOW_SCHEMA_DROP;
import static ch.eitchnet.db.DbConstants.PROP_ALLOW_SCHEMA_MIGRATION;

import java.util.Map;
import java.util.Map.Entry;

import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import li.strolch.runtime.configuration.StrolchConfiguration;
import li.strolch.runtime.configuration.StrolchConfigurationException;
import ch.eitchnet.db.DbConnectionCheck;
import ch.eitchnet.db.DbConnectionInfo;
import ch.eitchnet.db.DbException;
import ch.eitchnet.db.DbMigrationState;
import ch.eitchnet.db.DbSchemaVersionCheck;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeContext;

public class PostgreSqlDbInitializer extends PostgreSqlInitializer {

	private StrolchAgent agent;
	private PostgreSqlPersistenceHandler persistenceHandler;
	private RuntimeConfiguration runtimeConfig;
	private Certificate certificate;
	private boolean allowSchemaCreation;
	private boolean allowSchemaMigration;
	private boolean allowSchemaDrop;

	/**
	 * @param agent
	 * @param persistenceHandler
	 */
	public PostgreSqlDbInitializer(StrolchAgent agent, PostgreSqlPersistenceHandler persistenceHandler,
			ComponentConfiguration persistenceConfig) {
		super(agent, persistenceHandler);
		this.agent = agent;
		StrolchConfiguration strolchConfiguration = agent.getStrolchConfiguration();
		this.runtimeConfig = strolchConfiguration.getRuntimeConfiguration();
		this.persistenceHandler = persistenceHandler;
		this.allowSchemaCreation = persistenceConfig.getBoolean(PROP_ALLOW_SCHEMA_CREATION, Boolean.FALSE);
		this.allowSchemaMigration = persistenceConfig.getBoolean(PROP_ALLOW_SCHEMA_MIGRATION, Boolean.FALSE);
		this.allowSchemaDrop = persistenceConfig.getBoolean(PROP_ALLOW_SCHEMA_DROP, Boolean.FALSE);
	}

	@Override
	protected Certificate getCertificate() {
		return this.certificate;
	}

	@Override
	public void execute(PrivilegeContext privilegeContext) {
		this.certificate = privilegeContext.getCertificate();

		// first make sure we can connect to the database
		Map<String, DbConnectionInfo> connetionInfoMap = this.persistenceHandler.getConnetionInfoMap();
		DbConnectionCheck connectionCheck = new DbConnectionCheck(connetionInfoMap);
		try {
			connectionCheck.checkConnections();
		} catch (DbException e) {
			throw new StrolchConfigurationException("At least one of the configured DB connections is invalid: "
					+ e.getMessage(), e);
		}

		// first make sure that the data store files exist
		for (String realmName : this.agent.getContainer().getRealmNames()) {
			// throws exception if does not exist
			getDataStoreFile(this.runtimeConfig, this.realmConfig, realmName);
		}

		// for each connection info:
		// - make sure schema exists
		// - if it didn't exist, initialize with a set of data defined by the data store file
		DbSchemaVersionCheck schemaVersionCheck = new DbSchemaVersionCheck(PostgreSqlPersistenceHandler.SCRIPT_PREFIX,
				this.getClass(), this.allowSchemaCreation, this.allowSchemaMigration, this.allowSchemaDrop);
		for (Entry<String, DbConnectionInfo> entry : connetionInfoMap.entrySet()) {
			String realmName = entry.getKey();
			DbConnectionInfo connectionInfo = entry.getValue();
			StrolchRealm realm = this.agent.getContainer().getRealm(realmName);
			if (realm.getMode().isTransient())
				continue;

			// check that the schema exists
			DbMigrationState migrationType;
			try {
				migrationType = schemaVersionCheck.checkSchemaVersion(connectionInfo);
			} catch (DbException e) {
				throw new RuntimeException("Failed to validate schema for connection " + connectionInfo.getUrl(), e);
			}

			// now init the DB if needed
			initSchemaFromDataStore(migrationType, realmName);
		}
	}
}