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

import li.strolch.runtime.configuration.ConfigurationSaxParser.ConfigurationBuilder;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.XmlHelper;

import java.io.File;
import java.text.MessageFormat;

public class ConfigurationParser {

	public static final String STROLCH_CONFIGURATION_XML = "StrolchConfiguration.xml";

	public static StrolchConfiguration parseConfiguration(String environment, File configPathF, File dataPathF,
			File tempPathF) {
		DBC.PRE.assertNotEmpty("environment value must be set!", environment);
		DBC.PRE.assertNotNull("configPathF must be set!", configPathF);
		DBC.PRE.assertNotNull("dataPathF must be set!", dataPathF);
		DBC.PRE.assertNotNull("tempPathF must be set!", tempPathF);
		DBC.PRE.assertNotEquals("environment must be a value other than 'global'!", ConfigurationTags.ENV_GLOBAL,
				environment);

		// config path: readable directory
		if (!configPathF.isDirectory() || !configPathF.canRead()) {
			String msg = "Config path is not readable at {0}";
			msg = MessageFormat.format(msg, configPathF);
			throw new StrolchConfigurationException(msg);
		}

		// data path: writable directory
		if (!dataPathF.isDirectory() || !dataPathF.canRead() || !dataPathF.canWrite()) {
			String msg = "Data path is not a directory or readable or writeable at {0}";
			msg = MessageFormat.format(msg, dataPathF);
			throw new StrolchConfigurationException(msg);
		}

		// tmp path: writable directory
		if (!tempPathF.isDirectory() || !tempPathF.canRead() || !tempPathF.canWrite()) {
			String msg = "Temp path is not a directory or readable or writeable at {0}";
			msg = MessageFormat.format(msg, tempPathF);
			throw new StrolchConfigurationException(msg);
		}

		// get path to configuration file
		File configurationFile = new File(configPathF, STROLCH_CONFIGURATION_XML);
		if (!configurationFile.isFile() || !configurationFile.canRead()) {
			String msg = "Configuration file is not readable at {0}";
			msg = MessageFormat.format(msg, configurationFile);
			throw new StrolchConfigurationException(msg);
		}

		// parse the configuration file
		ConfigurationSaxParser configurationParser = new ConfigurationSaxParser(environment);
		XmlHelper.parseDocument(configurationFile, configurationParser);

		// merge global and selected environment
		ConfigurationBuilder globalEnvBuilder = configurationParser.getGlobalEnvBuilder();
		ConfigurationBuilder envBuilder = configurationParser.getEnvBuilder();
		if (envBuilder == null) {
			String msg = "The environment {0} does not exist!";
			msg = MessageFormat.format(msg, environment);
			throw new StrolchConfigurationException(msg);
		}
		globalEnvBuilder.merge(envBuilder);

		// build configuration

		return globalEnvBuilder.build(configPathF, dataPathF, tempPathF);
	}
}
