/*
 * Copyright (c) 2012
 * 
 * This file is part of ch.eitchnet.java.utils
 *
 * ch.eitchnet.java.utils is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * ch.eitchnet.java.utils is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ch.eitchnet.java.utils.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.utils.helper;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
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
	 * new file <user.dir>/tmp/ {@link Log4jConfigurator#FILE_LOG4J_TEMP} and then finally
	 * {@link PropertyConfigurator#configureAndWatch(String)} is called so that the configuration is loaded and log4j
	 * watches the temporary file for configuration changes
	 * </p>
	 */
	public static synchronized void loadLog4jConfiguration() {

		// get a configured log4j properties file, or use default
		// RSPConfigConstants.FILE_LOG4J
		String fileLog4j = SystemHelper.getProperty(Log4jConfigurator.class.getName(),
				Log4jConfigurator.PROP_FILE_LOG4J, Log4jConfigurator.FILE_LOG4J);

		// get the root directory
		String userDir = System.getProperty("user.dir");
		String configDir = userDir + "/config/";

		String pathNameToLog4j = configDir + fileLog4j;
		File log4JPath = new File(pathNameToLog4j);

		try {

			// load the log4j.properties file
			if (!log4JPath.exists())
				throw new RuntimeException("The log4j configuration file does not exist at "
						+ log4JPath.getAbsolutePath());

			// now perform the loading
			loadLog4jConfiguration(log4JPath);

		} catch (Exception e) {

			Log4jConfigurator.configure();
			logger.error(e, e);
			logger.error("Log4j COULD NOT BE INITIALIZED. Please check the log4j configuration file exists at "
					+ log4JPath.getAbsolutePath());

		}
	}

	/**
	 * <p>
	 * Loads the given log4j configuration
	 * </p>
	 * 
	 * <p>
	 * Any properties in the properties are substituted using
	 * {@link StringHelper#replaceProperties(Properties, Properties)} and then the configuration file is written to a
	 * new file <user.dir>/tmp/ {@link Log4jConfigurator#FILE_LOG4J_TEMP} and then finally
	 * {@link PropertyConfigurator#configureAndWatch(String)} is called so that the configuration is loaded and log4j
	 * watches the temporary file for configuration changes
	 * </p>
	 * 
	 * @param log4jConfigPath
	 */
	public static synchronized void loadLog4jConfiguration(File log4jConfigPath) {

		if (log4jConfigPath == null)
			throw new RuntimeException("log4jConfigPath may not be null!");

		// first clean up any old watch dog in case of a programmatic re-load of the configuration
		cleanupOldWatchdog();

		// get the root directory
		String userDir = System.getProperty("user.dir");
		String tmpDir = userDir + "/tmp/";

		String pathNameToLog4jTemp = tmpDir + Log4jConfigurator.FILE_LOG4J_TEMP;
		Properties log4jProperties = new Properties();
		FileInputStream fin = null;
		FileOutputStream fout = null;

		try {

			fin = new FileInputStream(log4jConfigPath);
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

			// XXX if the server is in a web context, then we may not use the
			// FileWatchDog
			BasicConfigurator.resetConfiguration();
			watchDog = new Log4jPropertyWatchDog(pathNameToLog4jTemp);
			watchDog.start();

			logger.info("Log4j is configured to use and watch file " + pathNameToLog4jTemp);

		} catch (Exception e) {

			Log4jConfigurator.configure();
			logger.error(e, e);
			logger.error("Log4j COULD NOT BE INITIALIZED. Please check the log4j configuration file at "
					+ log4jConfigPath);

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
	 * <p>
	 * Loads the log4j configuration file as a class resource by calling {@link Class#getResourceAsStream(String)} for
	 * the given class
	 * </p>
	 * 
	 * @param clazz
	 */
	public static synchronized void loadLog4jConfigurationAsResource(Class<?> clazz) {

		try {

			if (clazz == null)
				throw new RuntimeException("clazz may not be null!");

			InputStream resourceAsStream = clazz.getResourceAsStream("/" + FILE_LOG4J);
			if (resourceAsStream == null) {
				throw new RuntimeException("The resource '" + FILE_LOG4J + "' could not be found for class "
						+ clazz.getName());
			}

			// load the properties from the input stream
			Properties log4jProperties = new Properties();
			log4jProperties.load(resourceAsStream);

			// and then 
			loadLog4jConfiguration(log4jProperties);

		} catch (Exception e) {
			Log4jConfigurator.configure();
			logger.error(e, e);
			logger.error("Log4j COULD NOT BE INITIALIZED. Please check that the log4j configuration file '"
					+ FILE_LOG4J + "' exists as a resource for class " + clazz.getName()
					+ " and is a valid properties configuration");
		}
	}

	/**
	 * <p>
	 * Loads the given log4j configuration. Log4j is configured with the given properties. The only change is that
	 * {@link StringHelper#replaceProperties(Properties, Properties)} is used to replace any properties
	 * </p>
	 * 
	 * <p>
	 * No property watch dog is loaded
	 * </p>
	 * 
	 * @param log4jProperties
	 *            the properties to use for the log4j configuration
	 */
	public static synchronized void loadLog4jConfiguration(Properties log4jProperties) {

		try {

			if (log4jProperties == null)
				throw new RuntimeException("log4jProperties may not be null!");

			// first clean up any old watch dog in case of a programmatic re-load of the configuration
			cleanupOldWatchdog();

			// replace any variables
			StringHelper.replaceProperties(log4jProperties, System.getProperties());

			// now configure log4j
			PropertyConfigurator.configure(log4jProperties);
			logger.info("Log4j is configured using the given properties.");

		} catch (Exception e) {
			Log4jConfigurator.configure();
			logger.error(e, e);
			logger.error("Log4j COULD NOT BE INITIALIZED. The given log4jProperties seem not to be valid!");
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
