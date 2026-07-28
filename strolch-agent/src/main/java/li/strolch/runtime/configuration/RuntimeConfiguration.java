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
package li.strolch.runtime.configuration;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import li.strolch.model.Tags;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.text.MessageFormat;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import static li.strolch.model.Tags.Json.VERBOSE;

public class RuntimeConfiguration extends AbstractionConfiguration {

	private static final Logger logger = LoggerFactory.getLogger(RuntimeConfiguration.class);

	public static final String PROP_LOCALE = "locale";
	public static final String RUNTIME = "Runtime";
	public static final String PROP_TIMEZONE = "timezone";

	public static final String PROP_TEMP_RETENTION_DEFAULT = "temp.retention.default";
	public static final String PROP_TEMP_RETENTION_PREFIX = "temp.retention.";

	public static final String PROP_TEMP_RETENTION_KEEP_DEFAULT = "temp.retention.keep.default";
	public static final String PROP_TEMP_RETENTION_KEEP_PREFIX = "temp.retention.keep.";

	public static final String PROP_TEMP_RETENTION_DELETE_ENABLED = "temp.retention.delete.enabled";

	public static final String PROP_CLEAR_TEMP_PATH_IDS = "clear.temp.path.ids";
	public static final String PROP_CLEAR_TEMP_PATH_PREFIX = "clear.temp.path.";

	private final String applicationName;
	private final String environment;
	private final File configPath;
	private final File dataPath;
	private final File tempPath;

	private Locale locale;
	private Set<SupportedLanguage> supportedLanguages;

	public RuntimeConfiguration(String applicationName, String environment, Map<String, String> configurationValues,
			File configPathF, File dataPathF, File tempPathF, Set<SupportedLanguage> supportedLanguages) {
		super(RUNTIME, configurationValues,
				configurationValues.getOrDefault(VERBOSE, "false").equalsIgnoreCase("true"));

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

		this.applicationName = applicationName;
		this.environment = environment;

		this.configPath = configPathF;
		this.dataPath = dataPathF;
		this.tempPath = tempPathF;

		this.locale = Locale.forLanguageTag(getString(PROP_LOCALE, Locale.getDefault().toLanguageTag()));
		this.supportedLanguages = supportedLanguages;
	}

	public String getApplicationName() {
		return this.applicationName;
	}

	public String getEnvironment() {
		return this.environment;
	}

	public File getTempPath() {
		return this.tempPath;
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

	public void setLocale(Locale locale) {
		this.locale = locale;
	}

	public Set<SupportedLanguage> getSupportedLanguages() {
		return this.supportedLanguages;
	}

	public void setSupportedLanguages(Set<SupportedLanguage> supportedLanguages) {
		this.supportedLanguages = supportedLanguages;
	}

	public String getTimezone() {
		return getString(RuntimeConfiguration.PROP_TIMEZONE, System.getProperty("user.timezone"));
	}

	public boolean isVerbose() {
		return this.verbose;
	}

	public Duration getTempRetention(String prefix) {
		String key = PROP_TEMP_RETENTION_PREFIX + prefix;
		String value;
		if (hasProperty(key))
			value = getString(key, null);
		else
			value = getString(PROP_TEMP_RETENTION_DEFAULT, "P90D");

		try {
			return Duration.parse(value);
		} catch (Exception e) {
			logger.error("Failed to parse retention duration {} for prefix {}. Falling back to P90D.", value, prefix);
			return Duration.ofDays(90);
		}
	}

	public int getTempRetentionKeep(String prefix) {
		String key = PROP_TEMP_RETENTION_KEEP_PREFIX + prefix;
		if (hasProperty(key))
			return getInt(key, 0);
		return getInt(PROP_TEMP_RETENTION_KEEP_DEFAULT, 0);
	}

	public boolean isTempRetentionDeleteEnabled() {
		return getBoolean(PROP_TEMP_RETENTION_DELETE_ENABLED, false);
	}

	public List<String> getClearTempPathIds() {
		List<String> pathIds = new ArrayList<>();
		String value = getString(PROP_CLEAR_TEMP_PATH_IDS, null);
		if (value != null && !value.isEmpty()) {
			for (String idStr : value.split(",")) {
				idStr = idStr.trim();
				if (!idStr.isEmpty()) {
					pathIds.add(idStr);
				}
			}
		}
		return pathIds;
	}

	public File getClearTempPath(String pathId) {
		String key = PROP_CLEAR_TEMP_PATH_PREFIX + pathId + ".path";
		String value = getString(key, null);
		if (value == null || value.isEmpty()) {
			return null;
		}
		return new File(value);
	}

	public Duration getClearTempPathRetention(String pathId) {
		String key = PROP_CLEAR_TEMP_PATH_PREFIX + pathId + ".retention";
		String value = getString(key, "P90D");
		try {
			return Duration.parse(value);
		} catch (Exception e) {
			logger.error("Failed to parse retention duration {} for clear temp path id {}. Falling back to P90D.", value, pathId);
			return Duration.ofDays(90);
		}
	}

	public int getClearTempPathKeep(String pathId) {
		String key = PROP_CLEAR_TEMP_PATH_PREFIX + pathId + ".keep";
		return getInt(key, 0);
	}

	/**
	 * Returns the file in the config directory of the root of the application
	 *
	 * @param context     short name to define who requires this file for error handling
	 * @param fileName    the relative name of the config file to return
	 * @param checkExists if true, then an exception is thrown, using the context as info, if the config file does not
	 *                    exist
	 *
	 * @return the file in the config directory of the root of the application
	 */
	public File getConfigFile(String context, String fileName, boolean checkExists) {
		File configFile = new File(getConfigPath(), fileName);
		if (checkExists && (!configFile.isFile() || !configFile.canRead())) {
			String msg = "[{0}] requires config file from component {1} which does not exist with name: {2}";
			msg = MessageFormat.format(msg, getName(), context, fileName);
			throw new StrolchConfigurationException(msg);
		}
		return configFile;
	}

	/**
	 * Returns the file in the data directory of the root of the application
	 *
	 * @param context     short name to define who requires this file for error handling
	 * @param fileName    the relative name of the data file to return
	 * @param checkExists if true, then an exception is thrown, using the context as info, if the data file does not
	 *                    exist
	 *
	 * @return the file in the data directory of the root of the application
	 */
	public File getDataFile(String context, String fileName, boolean checkExists) {
		File dataFile = new File(getDataPath(), fileName);
		if (checkExists && (!dataFile.isFile() || !dataFile.canRead())) {
			String msg = "[{0}] requires data file from component {1} which does not exist with name: {2}";
			msg = MessageFormat.format(msg, getName(), context, fileName);
			throw new StrolchConfigurationException(msg);
		}
		return dataFile;
	}

	/**
	 * Returns the directory in the data directory of the root of the application
	 *
	 * @param context     short name to define who requires this directory for error handling
	 * @param dirName     the relative name of the data directory to return
	 * @param checkExists if true, then an exception is thrown, using the context as info, if the data directory does
	 *                    not exist
	 *
	 * @return the directory in the data directory of the root of the application
	 */
	public File getDataDir(String context, String dirName, boolean checkExists) {
		File dataDir = new File(getDataPath(), dirName);
		if (checkExists && (!dataDir.isDirectory() || !dataDir.canRead())) {
			String msg = "[{0}] requires data directory from component {1} which does not exist with name: {2}";
			msg = MessageFormat.format(msg, getName(), context, dirName);
			throw new StrolchConfigurationException(msg);
		}
		return dataDir;
	}

	@Override
	public JsonObject toJson() {
		JsonObject runtimeJ = super.toJson();

		runtimeJ.addProperty(Tags.Json.APPLICATION_NAME, applicationName);
		runtimeJ.addProperty(Tags.Json.ENVIRONMENT, environment);
		runtimeJ.addProperty(Tags.Json.CONFIG_PATH, configPath.getAbsolutePath());
		runtimeJ.addProperty(Tags.Json.DATA_PATH, dataPath.getAbsolutePath());
		runtimeJ.addProperty(Tags.Json.TEMP_PATH, tempPath.getAbsolutePath());
		runtimeJ.addProperty(Tags.Json.LOCALE, locale.toLanguageTag());
		runtimeJ.add(Tags.Json.SUPPORTED_LANGUAGES, supportedLanguages
				.stream()
				.map(SupportedLanguage::name)
				.collect(JsonArray::new, JsonArray::add, JsonArray::addAll));

		return runtimeJ;
	}
}
