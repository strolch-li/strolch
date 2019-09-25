package ${package};

import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchBootstrapper;
import li.strolch.runtime.configuration.StrolchEnvironment;
import li.strolch.utils.helper.StringHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Main {

	private static final Logger logger = LoggerFactory.getLogger(Main.class);
	private static final String APP_NAME = "${appName}";

	private static StrolchAgent agent;

	public static void main(String[] args) throws Throwable {

		logger.info("Starting " + APP_NAME + "...");
		long start = System.currentTimeMillis();
		try {
			// find environment from ENV.properties
			String env = StrolchEnvironment.getEnvironmentFromResourceEnv(Main.class);

			// and now start by system properties user.dir
			StrolchBootstrapper bootstrapper = new StrolchBootstrapper(Main.class);
			Main.agent = bootstrapper.setupByUserDir(env, "runtime");
			Main.agent.initialize();
			Main.agent.start();

		} catch (Throwable e) {
			logger.error("Failed to start " + APP_NAME + " due to: " + e.getMessage(), e);
			throw e;
		}

		Runtime.getRuntime().addShutdownHook(new Thread(() -> {
			try {
				Thread.sleep(200);
				System.out.println("Shutting down ...");
				Main.agent.stop();
				Main.agent.destroy();
			} catch (InterruptedException e) {
				logger.error("Failed to stop " + APP_NAME + " due to " + e.getMessage(), e);
			}
		}));

		long took = System.currentTimeMillis() - start;
		logger.info("Started " + APP_NAME + " in " + (StringHelper.formatMillisecondsDuration(took)));
	}
}