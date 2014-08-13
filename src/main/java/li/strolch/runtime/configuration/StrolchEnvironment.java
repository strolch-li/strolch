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
import java.io.FileInputStream;
import java.text.MessageFormat;
import java.util.Properties;

import li.strolch.runtime.StrolchConstants;
import ch.eitchnet.utils.dbc.DBC;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchEnvironment {

	public static final String ENV_PROPERTIES_FILE = "ENV.properties";

	public static String getEnvironmentFromSystemProperties() {
		String environment = System.getProperties().getProperty(StrolchConstants.ENV_STROLCH);
		if (StringHelper.isEmpty(environment)) {
			String msg = "The system property {0} is missing!";
			throw new StrolchConfigurationException(MessageFormat.format(msg, StrolchConstants.ENV_STROLCH));
		}

		return environment;
	}

	public static String getEnvironmentFromEnvProperties(File rootPath) {
		File envF = new File(rootPath, ENV_PROPERTIES_FILE);
		DBC.PRE.assertExists(ENV_PROPERTIES_FILE + " does not exist in " + rootPath.getAbsolutePath(), envF);
		Properties envP = new Properties();
		try (FileInputStream fin = new FileInputStream(envF)) {
			envP.load(fin);
		} catch (Exception e) {
			throw new StrolchConfigurationException("Failed to load " + ENV_PROPERTIES_FILE + " in " + rootPath, e);
		}

		String environment = envP.getProperty(StrolchConstants.ENV_STROLCH);
		if (StringHelper.isEmpty(environment)) {
			String msg = "The property {0} does not exist in {1}";
			msg = MessageFormat.format(msg, StrolchConstants.ENV_STROLCH, envF.getAbsolutePath());
			throw new StrolchConfigurationException(msg);
		}

		return environment;
	}
}
