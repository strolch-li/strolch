package ch.eitchnet.utils.helper;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Date;
import java.util.Properties;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.apache.log4j.PropertyConfigurator;

/**
 * A simple configurator to configure log4j, with fall back default configuration
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class Log4jConfigurator {

	/**
	 * system property used to override the log4j configuration file
	 */
	public static final String PROP_FILE_LOG4J = "rsp.log4j.properties";

	/**
	 * default log4j configuration file
	 */
	public static final String FILE_LOG4J = "log4j.properties";

	/**
	 * runtime log4j configuration file which is a copy of the original file but has any place holders overwritten
	 */
	public static final String FILE_LOG4J_TEMP = "log4j.properties.tmp";

	private static final Logger logger = Logger.getLogger(Log4jConfigurator.class);
	private static Log4jPropertyWatchDog watchDog;

	/**
	 * Configures log4j with the default {@link ConsoleAppender}
	 */
	public static synchronized void configure() {
		cleanupOldWatchdog();
		BasicConfigurator.resetConfiguration();
		BasicConfigurator.configure(new ConsoleAppender(getDefaulLayout()));
		Logger.getRootLogger().setLevel(Level.INFO);
	}

	/**
	 * Returns the default layout: %d %5p [%t] %C{1} %M - %m%n
	 * 
	 * @return the default layout
	 */
	public static PatternLayout getDefaulLayout() {
		return new PatternLayout("%d %5p [%t] %C{1} %M - %m%n");
	}

	/**
	 * <p>
	 * Loads the log4j configuration
	 * </p>
	 * 
	 * <p>
	 * This file is configurable through the {@link Log4jConfigurator#PROP_FILE_LOG4J} system property, but uses the
	 * default {@link Log4jConfigurator#FILE_LOG4J} file, if no configuration option is set. The path used is
	 * <user.dir>/config/ whereas <user.dir> is a system property
	 * </p>
	 * 
	 * <p>
	 * Any properties in the properties are substituted using
	 * {@link StringHelper#replaceProperties(Properties, Properties)} and then the configuration file is written to a
	 * new file <user.dir>/tmp/{@link Log4jConfigurator#FILE_LOG4J_TEMP} and then finally
	 * {@link PropertyConfigurator#configureAndWatch(String)} is called so that the configuration is loaded and log4j
	 * watches the temporary file for configuration changes
	 * </p>
	 */
	public static synchronized void loadLog4jConfiguration() {

		// first clean up any old watch dog in case of a programmatic re-load of hte configuration
		cleanupOldWatchdog();

		// get a configured log4j properties file, or use default RSPConfigConstants.FILE_LOG4J
		String fileLog4j = SystemHelper.getProperty(Log4jConfigurator.class.getName(),
				Log4jConfigurator.PROP_FILE_LOG4J, Log4jConfigurator.FILE_LOG4J);

		// get the root directory
		String userDir = System.getProperty("user.dir");
		String configDir = userDir + "/config/";
		String tmpDir = userDir + "/tmp/";

		// load the log4j.properties file
		String pathNameToLog4j = configDir + fileLog4j;
		File log4JPath = new File(pathNameToLog4j);
		if (!log4JPath.exists())
			throw new RuntimeException("The log4j configuration file does not exist at " + log4JPath.getAbsolutePath());

		String pathNameToLog4jTemp = tmpDir + Log4jConfigurator.FILE_LOG4J_TEMP;
		Properties log4jProperties = new Properties();
		FileInputStream fin = null;
		FileOutputStream fout = null;
		try {
			fin = new FileInputStream(pathNameToLog4j);
			log4jProperties.load(fin);
			fin.close();

			// replace any variables
			StringHelper.replaceProperties(log4jProperties, System.getProperties());

			// write this as the temporary log4j file
			File logsFileDir = new File(tmpDir);
			if (!logsFileDir.exists() && !logsFileDir.mkdirs())
				throw new RuntimeException("Could not create log path " + logsFileDir.getAbsolutePath());

			fout = new FileOutputStream(pathNameToLog4jTemp);
			log4jProperties.store(fout, "Running instance log4j configuration " + new Date());
			fout.close();

			// XXX if the server is in a web context, then we may not use the FileWatchDog
			BasicConfigurator.resetConfiguration();
			watchDog = new Log4jPropertyWatchDog(pathNameToLog4jTemp);
			watchDog.start();

			logger.info("Log4j is configured to use and watch file " + pathNameToLog4jTemp);

		} catch (Exception e) {
			Log4jConfigurator.configure();
			logger.error(e, e);
			logger.error("Log4j COULD NOT BE INITIALIZED. Please check the " + fileLog4j + " file at "
					+ pathNameToLog4j);
		} finally {
			if (fin != null) {
				try {
					fin.close();
				} catch (IOException e) {
					logger.error("Exception closing input file: " + e, e);
				}
			}
			if (fout != null) {
				try {
					fout.close();
				} catch (IOException e) {
					logger.error("Exception closing output file: " + e, e);
				}
			}
		}
	}

	/**
	 * Cleanup a running watch dog
	 */
	public static synchronized void cleanupOldWatchdog() {
		// clean up an old watch dog
		if (watchDog != null) {
			logger.info("Stopping old Log4j watchdog.");
			watchDog.interrupt();
			try {
				watchDog.join(1000l);
			} catch (InterruptedException e) {
				logger.error("Oops. Could not terminate an old WatchDog.");
			} finally {
				watchDog = null;
			}
			logger.info("Done.");
		}
	}
}
