package li.strolch.persistence.postgresql;

import static li.strolch.db.DbConstants.*;

import javax.sql.DataSource;
import java.sql.Connection;
import java.util.Date;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.db.DbSchemaVersionCheck;
import li.strolch.persistence.api.*;
import li.strolch.runtime.configuration.ComponentConfiguration;

public class PostgreSqlDataArchiveHandler extends StrolchComponent implements DataArchiveHandler {

	private static final String SCRIPT_PREFIX = "archive";

	public PostgreSqlDataArchiveHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
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
	public Connection getConnection(StrolchTransaction tx) {
		PostgreSqlPersistenceHandler persistenceHandler = (PostgreSqlPersistenceHandler) getComponent(
				PersistenceHandler.class);
		return persistenceHandler.getConnection(tx.getRealmName());
	}

	@Override
	public void run(StrolchTransaction tx, BiConsumer<Connection, TransactionResult> runnable) {
		try (Connection connection = getConnection(tx)) {
			TransactionResult txResult = new TransactionResult(tx.getRealmName(), System.nanoTime(), new Date());
			runnable.accept(connection, txResult);
			connection.commit();
		} catch (Exception e) {
			throw new StrolchPersistenceException("Archive DB Connection failed", e);
		}
	}

	@Override
	public <T> T runWithResult(StrolchTransaction tx, BiFunction<Connection, TransactionResult, T> runnable) {
		try (Connection connection = getConnection(tx)) {
			TransactionResult txResult = new TransactionResult(tx.getRealmName(), System.nanoTime(), new Date());
			T t = runnable.apply(connection, txResult);
			connection.commit();
			return t;
		} catch (Exception e) {
			throw new StrolchPersistenceException("Archive DB Connection failed", e);
		}
	}

	@Override
	public OrderDao getOrderDao(Connection connection, TransactionResult txResult) {
		return new ArchivePostgreSqlOrderDao(DataType.xml, connection, txResult, false);
	}

	@Override
	public ResourceDao getResourceDao(Connection connection, TransactionResult txResult) {
		return new ArchivePostgreSqlResourceDao(DataType.xml, connection, txResult, false);
	}

	@Override
	public ActivityDao getActivityDao(Connection connection, TransactionResult txResult) {
		return new ArchivePostgreSqlActivityDao(DataType.xml, connection, txResult, false);
	}
}
