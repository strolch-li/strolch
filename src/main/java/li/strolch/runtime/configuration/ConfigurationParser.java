/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.runtime.configuration;

import java.io.File;
import java.text.MessageFormat;

import li.strolch.runtime.configuration.ConfigurationSaxParser.ConfigurationBuilder;
import ch.eitchnet.utils.helper.XmlHelper;

public class ConfigurationParser {

	public static final String STROLCH_CONFIGURATION_XML = "StrolchConfiguration.xml"; //$NON-NLS-1$

	// private static final Logger logger = LoggerFactory.getLogger(ConfigurationParser.class);

	public static StrolchConfiguration parseConfiguration(File rootPathF) {

		if (!rootPathF.isDirectory() || !rootPathF.canRead()) {
			String msg = "Root path is not readable at {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, rootPathF);
			throw new StrolchConfigurationException(msg);
		}

		File configPathF = new File(rootPathF, RuntimeConfiguration.PATH_CONFIG);
		if (!configPathF.isDirectory() || !configPathF.canRead()) {
			String msg = "Config path is not readable at {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, configPathF);
			throw new StrolchConfigurationException(msg);
		}

		File configurationFile = new File(configPathF, STROLCH_CONFIGURATION_XML);
		if (!configurationFile.isFile() || !configurationFile.canRead()) {
			String msg = "Configuration file is not readable at {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, configurationFile);
			throw new StrolchConfigurationException(msg);
		}

		// parse the configuration file
		ConfigurationSaxParser configurationParser = new ConfigurationSaxParser();
		XmlHelper.parseDocument(configurationFile, configurationParser);
		ConfigurationBuilder configurationBuilder = configurationParser.getConfigurationBuilder();

		StrolchConfiguration strolchConfiguration = configurationBuilder.build(rootPathF);
		return strolchConfiguration;
	}
}
