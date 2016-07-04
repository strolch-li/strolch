package li.strolch.minimal.rest.main;

import java.io.File;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchBootstrapper;
import li.strolch.runtime.configuration.StrolchEnvironment;

@WebListener
public class StartupListener implements ServletContextListener {

	private static final Logger logger = LoggerFactory.getLogger(StartupListener.class);

	private StrolchAgent agent;

	@Override
	public void contextInitialized(ServletContextEvent sce) {

		String realPath = sce.getServletContext().getRealPath("/WEB-INF");

		File pathF = new File(realPath);
		String environment = StrolchEnvironment.getEnvironmentFromEnvProperties(pathF);
		logger.info("Starting Strolch Minimal Rest...");
		try {
			this.agent = new StrolchBootstrapper(StartupListener.class).setupByRoot(environment, pathF);
			this.agent.initialize();
			this.agent.start();
		} catch (Exception e) {
			logger.error("Failed to start Strolch due to: " + e.getMessage(), e);
			throw e;
		}

		logger.info("Started Strolch Minimal Rest.");
	}

	@Override
	public void contextDestroyed(ServletContextEvent sce) {
		if (this.agent != null) {
			this.agent.stop();
			this.agent.destroy();
		}
		logger.info("Destroyed Strolch Minimal rest.");
	}
}
