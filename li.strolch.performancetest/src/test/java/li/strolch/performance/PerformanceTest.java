package li.strolch.performance;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;

import li.strolch.db.DbSchemaVersionCheck;
import li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler;
import li.strolch.privilege.model.Certificate;
import li.strolch.service.api.ServiceHandler;
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

	protected void runPerformanceTest(String username) {
		Certificate certificate = runtime().getPrivilegeHandler().authenticate(username, username.toCharArray());
		ServiceHandler svcHandler = runtime().getServiceHandler();
		svcHandler.doService(certificate, new PerformanceTestService(), argInstance());
	}

	protected void runParallelPerformanceTest(String username) {

		int nrOfTasks = 5;

		ForkJoinPool commonPool = ForkJoinPool.commonPool();

		long start = System.currentTimeMillis();
		List<ForkJoinTask<Long>> tasks = new ArrayList<>();
		for (int i = 0; i < nrOfTasks; i++) {
			PerformanceTask task = new PerformanceTask(username);
			tasks.add(task);
			commonPool.execute(task);
		}

		logger.info("Executing " + tasks.size() + " tasks...");

		List<Long> results = new ArrayList<>();
		for (ForkJoinTask<Long> task : tasks) {
			results.add(task.join());
		}
		logger.info("Executed " + tasks.size() + " tasks.");
		for (int i = 0; i < results.size(); i++) {
			logger.info("Task " + i + " executed " + results.get(i) + " TXs");
		}

		long avg = (long) results.stream().mapToLong(l -> l).average().getAsDouble();
		long took = System.currentTimeMillis() - start;
		long txPerSec = avg / (took / 1000);
		logger.info("Average TXs was " + avg + " with " + txPerSec + " TXs/s");
	}

	public class PerformanceTask extends ForkJoinTask<Long> {

		private String username;
		private long nrOfTxs;

		public PerformanceTask(String username) {
			this.username = username;
		}

		@Override
		public Long getRawResult() {
			return this.nrOfTxs;
		}

		@Override
		protected void setRawResult(Long value) {
			// ignore
		}

		@Override
		protected boolean exec() {

			Certificate certificate = runtime().getPrivilegeHandler()
					.authenticate(username, this.username.toCharArray());
			ServiceHandler svcHandler = runtime().getServiceHandler();
			PerformanceTestResult svcResult = svcHandler
					.doService(certificate, new PerformanceTestService(), new PerformanceTestArgument());
			runtime().getPrivilegeHandler().invalidate(certificate);

			this.nrOfTxs = svcResult.getNrOfTxs();

			return true;
		}
	}
}
