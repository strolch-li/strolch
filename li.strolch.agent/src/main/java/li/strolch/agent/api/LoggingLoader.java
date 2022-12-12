package li.strolch.agent.api;

import java.io.File;
import java.net.MalformedURLException;

import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.util.ContextInitializer;
import ch.qos.logback.core.joran.spi.JoranException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggingLoader {

	private static final Logger logger = LoggerFactory.getLogger(LoggingLoader.class);

	private static final String LOGBACK_XML = "logback.xml";

	public static void reloadLogging(File configPathF) {

		File logConfigFile = new File(configPathF, LOGBACK_XML);
		if (!logConfigFile.exists()) {
			logger.info(
					"Not changing logback configuration as " + logConfigFile.getAbsolutePath() + " does not exist.");
		} else {
			if (!(LoggerFactory.getILoggerFactory() instanceof LoggerContext loggerContext)) {
				logger.error(logConfigFile.getAbsolutePath()
						+ "  exists, but LoggerFactory is not instance of ch.qos.logback.classic.LoggerContext. Ignoring.");
			} else {
				logger.info(logConfigFile.getAbsolutePath() + " file exists. Reloading logging configuration from "
						+ logConfigFile);
				try {
					loggerContext.reset();
					new ContextInitializer(loggerContext).configureByResource(logConfigFile.toURI().toURL());
					logger.info("Reloaded logger configuration from " + logConfigFile.getAbsolutePath());
				} catch (MalformedURLException | JoranException e) {
					try {
						new ContextInitializer(loggerContext).autoConfig();
					} catch (JoranException e1) {
						logger.error("Failed to reload original config after failure to load new config from "
								+ logConfigFile.getAbsolutePath(), e);
					}
					logger.error("Failed to reload logback configuration from file " + logConfigFile, e);
				}
			}
		}
	}
}
