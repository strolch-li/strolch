package li.strolch.persistence.postgresql;

import static li.strolch.db.DbConstants.*;
import static li.strolch.persistence.postgresql.DataType.xml;

import javax.sql.DataSource;
import java.sql.Connection;
import java.util.Date;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.db.DbSchemaVersionCheck;
import li.strolch.persistence.api.*;
import li.strolch.runtime.configuration.ComponentConfiguration;

public class PostgreSqlDataArchiveHandler extends StrolchComponent implements DataArchiveHandler {

	private static final String SCRIPT_PREFIX = "archive";

	private final Map<Connection, ResourceDao> resourceDaoMap;
	private final Map<Connection, OrderDao> orderDaoMap;
	private final Map<Connection, ActivityDao> activityDaoMap;

	public PostgreSqlDataArchiveHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
		this.resourceDaoMap = new ConcurrentHashMap<>();
		this.orderDaoMap = new ConcurrentHashMap<>();
		this.activityDaoMap = new ConcurrentHashMap<>();
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
		Connection con = null;
		try (Connection connection = getConnection(tx)) {
			con = connection;
			TransactionResult txResult = new TransactionResult(tx.getRealmName(), System.nanoTime(), new Date());
			runnable.accept(connection, txResult);
			flush(connection);
			connection.commit();
		} catch (Exception e) {
			throw new StrolchPersistenceException("Archive DB Connection failed", e);
		} finally {
			if (con != null)
				clearCachedDAOs(con);
		}
	}

	@Override
	public <T> T runWithResult(StrolchTransaction tx, BiFunction<Connection, TransactionResult, T> runnable) {
		Connection con = null;
		try (Connection connection = getConnection(tx)) {
			con = connection;
			TransactionResult txResult = new TransactionResult(tx.getRealmName(), System.nanoTime(), new Date());
			T t = runnable.apply(connection, txResult);
			flush(connection);
			connection.commit();
			return t;
		} catch (Exception e) {
			throw new StrolchPersistenceException("Archive DB Connection failed", e);
		} finally {
			if (con != null)
				clearCachedDAOs(con);
		}
	}

	private void flush(Connection connection) {
		ResourceDao resourceDao = this.resourceDaoMap.remove(connection);
		if (resourceDao != null)
			resourceDao.flush();
		OrderDao orderDao = this.orderDaoMap.remove(connection);
		if (orderDao != null)
			orderDao.flush();
		ActivityDao activityDao = this.activityDaoMap.remove(connection);
		if (activityDao != null)
			activityDao.flush();
	}

	private void clearCachedDAOs(Connection con) {
		this.resourceDaoMap.remove(con);
		this.orderDaoMap.remove(con);
		this.activityDaoMap.remove(con);
	}

	@Override
	public ResourceDao getResourceDao(Connection connection, TransactionResult txResult) {
		return this.resourceDaoMap.computeIfAbsent(connection,
				c -> new ArchivePostgreSqlResourceDao(xml, connection, txResult, false));
	}

	@Override
	public OrderDao getOrderDao(Connection connection, TransactionResult txResult) {
		return this.orderDaoMap.computeIfAbsent(connection,
				c -> new ArchivePostgreSqlOrderDao(xml, connection, txResult, false));
	}

	@Override
	public ActivityDao getActivityDao(Connection connection, TransactionResult txResult) {
		return this.activityDaoMap.computeIfAbsent(connection,
				c -> new ArchivePostgreSqlActivityDao(xml, connection, txResult, false));
	}
}
