/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package li.strolch.agent.api;

import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.util.ContextInitializer;
import ch.qos.logback.classic.util.DefaultJoranConfigurator;
import ch.qos.logback.core.joran.spi.JoranException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;

public class LoggingLoader {

	private static final Logger logger = LoggerFactory.getLogger(LoggingLoader.class);

	private static final String LOGBACK_XML = "logback.xml";

	private static File lastConfigFile;

	public static void reloadLogging(File configPathF) {
		File logConfigFile = new File(configPathF, LOGBACK_XML);
		if (!logConfigFile.exists())
			return;

		if (!(LoggerFactory.getILoggerFactory() instanceof LoggerContext loggerContext)) {
			logger.error(
					"{}  exists, but LoggerFactory is not instance of ch.qos.logback.classic.LoggerContext. Ignoring.",
					logConfigFile.getAbsolutePath());
		} else {
			logger.info("{} file exists. Reloading logging configuration from {}", logConfigFile.getAbsolutePath(),
					logConfigFile);
			try {
				loggerContext.reset();
				DefaultJoranConfigurator configurator = new DefaultJoranConfigurator();
				configurator.setContext(loggerContext);
				configurator.configureByResource(logConfigFile.toURI().toURL());
				logger.info("Reloaded logger configuration from {}", logConfigFile.getAbsolutePath());
				lastConfigFile = logConfigFile;
			} catch (Exception e) {
				try {
					new ContextInitializer(loggerContext).autoConfig();
				} catch (JoranException e1) {
					logger.error("Failed to reload original config after failure to load new config from {}",
							logConfigFile.getAbsolutePath(), e);
				}
				logger.error("Failed to reload logback configuration from file {}", logConfigFile, e);
			}
		}
	}

	public static void reloadLoggingConfiguration() {
		if (lastConfigFile != null) {
			logger.info("Reloading configuration from last config file {}", lastConfigFile.getAbsolutePath());
			System.out.println("Reloading configuration from last config file " + lastConfigFile.getAbsolutePath());
			reloadLogging(lastConfigFile.getParentFile());
		} else {
			if (!(LoggerFactory.getILoggerFactory() instanceof LoggerContext loggerContext)) {
				logger.error("LoggerFactory is not instance of {}. Ignoring request to reload configuration!",
						LoggerContext.class.getName());
				System.out.println("LoggerFactory is not instance of "
						+ LoggerContext.class.getName()
						+ ". Ignoring request to reload configuration!");
			} else {
				logger.info(
						"Resetting logging configuration using auto config as no previous config fila available...");
				System.out.println(
						"Resetting logging configuration using auto config as no previous config fila available...");
				try {
					new ContextInitializer(loggerContext).autoConfig();
				} catch (JoranException e) {
					logger.error("Failed to do logging auto configuration", e);
					System.out.println("Failed to do logging auto configuration");
					//noinspection CallToPrintStackTrace
					e.printStackTrace();
				}
			}
		}
	}

	public static void reset() {
		if (!(LoggerFactory.getILoggerFactory() instanceof LoggerContext loggerContext)) {
			logger.error("LoggerFactory is not instance of ch.qos.logback.classic.LoggerContext. Ignoring.");
		} else {
			logger.info("Resetting LoggerFactory {}", loggerContext);
			loggerContext.reset();
		}
	}
}
