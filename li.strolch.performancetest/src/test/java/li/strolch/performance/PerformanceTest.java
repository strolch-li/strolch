package li.strolch.performance;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.text.MessageFormat;

import li.strolch.db.DbSchemaVersionCheck;
import li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler;
import li.strolch.testbase.runtime.RuntimeMock;
import li.strolch.utils.Version;
import li.strolch.utils.helper.FileHelper;
import li.strolch.utils.helper.StringHelper;
import org.postgresql.Driver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class PerformanceTest {

	protected static final Logger logger = LoggerFactory.getLogger(PerformanceTest.class);

	private static RuntimeMock runtimeMock;

	protected RuntimeMock runtime() {
		return runtimeMock;
	}

	protected PerformanceTestArgument argInstance() {
		return new PerformanceTestArgument();
	}

	public static void buildRuntime(String sourcePath, String targetPath) {
		File configSrc = new File(sourcePath);
		File rootPath = new File(targetPath);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		runtimeMock.startContainer();
	}

	public static void dropSchema(String dbUrl, String dbUsername, String dbPassword) throws Exception {

		if (!Driver.isRegistered())
			Driver.register();

		Version dbVersion = DbSchemaVersionCheck
				.getExpectedDbVersion(PostgreSqlPersistenceHandler.SCRIPT_PREFIX, PostgreSqlPersistenceHandler.class);
		logger.info(MessageFormat.format("Dropping schema for expected version {0}", dbVersion));
		String sql = DbSchemaVersionCheck
				.getSql(PostgreSqlPersistenceHandler.SCRIPT_PREFIX, PostgreSqlPersistenceHandler.class, dbVersion,
						"drop"); //$NON-NLS-1$
		logger.info(StringHelper.NEW_LINE + sql);
		try (Connection connection = DriverManager.getConnection(dbUrl, dbUsername, dbPassword)) {
			connection.prepareStatement(sql).execute();
		}
	}

	public static void afterClass(String targetPath) throws Exception {
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();

		File rootPath = new File(targetPath);
		if (rootPath.exists()) {
			FileHelper.deleteFile(rootPath, false);
		}

		if (Driver.isRegistered())
			Driver.deregister();
	}
}
