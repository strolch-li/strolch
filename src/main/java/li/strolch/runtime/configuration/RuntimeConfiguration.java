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
import java.util.Locale;
import java.util.Map;

public class RuntimeConfiguration extends AbstractionConfiguration {

	public static final String PROP_LOCALE = "locale"; //$NON-NLS-1$
	public static final String RUNTIME = "Runtime"; //$NON-NLS-1$
	public static final String PATH_CONFIG = "config"; //$NON-NLS-1$
	public static final String PATH_DATA = "data"; //$NON-NLS-1$

	private final String applicationName;
	private final String environment;
	private final File rootPath;
	private final File configPath;
	private final File dataPath;

	private Locale locale;

	public RuntimeConfiguration(String applicationName, String environment, Map<String, String> configurationValues,
			File rootPathF) {
		super(RUNTIME, configurationValues);

		if (!rootPathF.isDirectory() || !rootPathF.canRead()) {
			String msg = "Root path is not readable at {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, rootPathF.getAbsolutePath());
			throw new StrolchConfigurationException(msg);
		}

		File configPathF = new File(rootPathF, PATH_CONFIG);
		if (!configPathF.isDirectory() || !configPathF.canRead()) {
			String msg = "Config path is not readable at {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, configPathF);
			throw new StrolchConfigurationException(msg);
		}

		File dataPathF = new File(rootPathF, PATH_DATA);
		if (!dataPathF.exists() && !dataPathF.mkdir()) {
			String msg = "Could not create missing data path at {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, configPathF);
			throw new StrolchConfigurationException(msg);
		}
		if (!dataPathF.isDirectory() || !dataPathF.canRead() || !dataPathF.canWrite()) {
			String msg = "Data path is not a directory or readable or writeable at {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, configPathF);
			throw new StrolchConfigurationException(msg);
		}

		this.applicationName = applicationName;
		this.environment = environment;

		this.rootPath = rootPathF;
		this.configPath = configPathF;
		this.dataPath = dataPathF;

		this.locale = new Locale(getString(PROP_LOCALE, Locale.getDefault().toString()));
	}

	public String getApplicationName() {
		return this.applicationName;
	}

	public String getEnvironment() {
		return this.environment;
	}

	public File getRootPath() {
		return this.rootPath;
	}

	public File getConfigPath() {
		return this.configPath;
	}

	public File getDataPath() {
		return this.dataPath;
	}

	public Locale getLocale() {
		return this.locale;
	}

	/**
	 * Returns the file in the config directory of the root of the application
	 * 
	 * @param context
	 *            short name to define who requires this file for error handling
	 * @param fileName
	 *            the relative name of the config file to return
	 * @param checkExists
	 *            if true, then an exception is thrown, using the context as info, if the config file does not exist
	 * 
	 * @return the file in the config directory of the root of the application
	 */
	public File getConfigFile(String context, String fileName, boolean checkExists) {
		File configFile = new File(getDataPath(), fileName);
		if (checkExists && !configFile.isFile() || !configFile.canRead()) {
			String msg = "[{0}] requires config file which does not exist with name: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, getName(), context, fileName);
			throw new StrolchConfigurationException(msg);
		}
		return configFile;
	}

	/**
	 * Returns the file in the data directory of the root of the application
	 * 
	 * @param context
	 *            short name to define who requires this file for error handling
	 * @param fileName
	 *            the relative name of the data file to return
	 * @param checkExists
	 *            if true, then an exception is thrown, using the context as info, if the data file does not exist
	 * 
	 * @return the file in the data directory of the root of the application
	 */
	public File getDataFile(String context, String fileName, boolean checkExists) {
		File dataFile = new File(getDataPath(), fileName);
		if (checkExists && !dataFile.isFile() || !dataFile.canRead()) {
			String msg = "[{0}] requires data file which does not exist with name: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, getName(), context, fileName);
			throw new StrolchConfigurationException(msg);
		}
		return dataFile;
	}

	/**
	 * Returns the directory in the data directory of the root of the application
	 * 
	 * @param context
	 *            short name to define who requires this directory for error handling
	 * @param dirName
	 *            the relative name of the data directory to return
	 * @param checkExists
	 *            if true, then an exception is thrown, using the context as info, if the data directory does not exist
	 * 
	 * @return the directory in the data directory of the root of the application
	 */
	public File getDataDir(String context, String dirName, boolean checkExists) {
		File dataDir = new File(getDataPath(), dirName);
		if (checkExists && !dataDir.isDirectory() || !dataDir.canRead()) {
			String msg = "[{0}] requires data directory which does not exist with name: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, getName(), context, dirName);
			throw new StrolchConfigurationException(msg);
		}
		return dataDir;
	}
}
