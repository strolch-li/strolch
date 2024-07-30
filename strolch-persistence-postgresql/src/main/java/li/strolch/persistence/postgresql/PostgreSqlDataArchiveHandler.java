package li.strolch.persistence.postgresql;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.db.DbSchemaVersionCheck;
import li.strolch.persistence.api.*;
import li.strolch.runtime.configuration.ComponentConfiguration;

import javax.sql.DataSource;
import java.sql.Connection;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Function;

import static li.strolch.db.DbConstants.*;
import static li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler.DATA_TYPE_XML;
import static li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler.PROP_DATA_TYPE;

public class PostgreSqlDataArchiveHandler extends StrolchComponent implements DataArchiveHandler {

	private static final String SCRIPT_PREFIX = "archive";
	private DataType dataType;

	public PostgreSqlDataArchiveHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {
		this.dataType = DataType.valueOf(configuration.getString(PROP_DATA_TYPE, DATA_TYPE_XML).toLowerCase());
		super.initialize(configuration);
	}

	@Override
	public void start() throws Exception {

		PostgreSqlPersistenceHandler persistenceHandler = (PostgreSqlPersistenceHandler) getComponent(
				PersistenceHandler.class);
		Map<String, DataSource> dsMap = persistenceHandler.getDataSources();

		ComponentConfiguration configuration = persistenceHandler.getConfiguration();

		boolean allowSchemaCreation = configuration.getBoolean(PROP_ALLOW_SCHEMA_CREATION, Boolean.FALSE);
		boolean allowSchemaMigration = configuration.getBoolean(PROP_ALLOW_SCHEMA_MIGRATION, Boolean.FALSE);
		boolean allowSchemaDrop = configuration.getBoolean(PROP_ALLOW_SCHEMA_DROP, Boolean.FALSE);

		DbSchemaVersionCheck schemaVersionCheck = new DbSchemaVersionCheck(SCRIPT_PREFIX, this.getClass(),
				allowSchemaCreation, allowSchemaMigration, allowSchemaDrop);
		schemaVersionCheck.checkSchemaVersion(dsMap);

		super.start();
	}

	@Override
	public ArchiveTransaction openArchiveTx(StrolchTransaction tx) {
		return new PostgreSqlArchiveTransaction(tx, this, ((PostgreSqlStrolchTransaction) tx).getConnection());
	}

	@Override
	public void run(StrolchTransaction tx, Consumer<ArchiveTransaction> runnable) {
		try (ArchiveTransaction archiveTx = openArchiveTx(tx)) {
			runnable.accept(archiveTx);
		} catch (Exception e) {
			throw new StrolchPersistenceException("Archive DB Connection failed", e);
		}
	}

	@Override
	public <T> T runWithResult(StrolchTransaction tx, Function<ArchiveTransaction, T> runnable) {
		try (ArchiveTransaction archiveTx = openArchiveTx(tx)) {
			return runnable.apply(archiveTx);
		} catch (Exception e) {
			throw new StrolchPersistenceException("Archive DB Connection failed", e);
		}
	}

	@Override
	public OrderDao getOrderDao(ArchiveTransaction archiveTx) {
		Connection connection = ((PostgreSqlArchiveTransaction) archiveTx).getConnection();
		return new ArchivePostgreSqlOrderDao(this.dataType, connection, archiveTx.getTxResult(), false);
	}

	@Override
	public ResourceDao getResourceDao(ArchiveTransaction archiveTx) {
		Connection connection = ((PostgreSqlArchiveTransaction) archiveTx).getConnection();
		return new ArchivePostgreSqlResourceDao(this.dataType, connection, archiveTx.getTxResult(), false);
	}

	@Override
	public ActivityDao getActivityDao(ArchiveTransaction archiveTx) {
		Connection connection = ((PostgreSqlArchiveTransaction) archiveTx).getConnection();
		return new ArchivePostgreSqlActivityDao(this.dataType, connection, archiveTx.getTxResult(), false);
	}
}
