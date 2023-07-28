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

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import li.strolch.model.StrolchValueType;
import li.strolch.model.Tags;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.nio.file.Path;
import java.text.MessageFormat;
import java.util.*;

import static java.util.Comparator.comparing;
import static li.strolch.utils.helper.StringHelper.isEmpty;

public abstract class AbstractionConfiguration {

	private static final Logger logger = LoggerFactory.getLogger(AbstractionConfiguration.class);
	public static final String SECRET = "Secret";

	private final String name;
	private final Map<String, String> configurationValues;
	private final Map<String, String> defaultValues;
	private final Map<String, String> valueTypes;

	public AbstractionConfiguration(String name, Map<String, String> configurationValues) {
		this.name = name;
		this.configurationValues = configurationValues;
		this.defaultValues = new HashMap<>();
		this.valueTypes = new HashMap<>();
	}

	public String getName() {
		return this.name;
	}

	public Properties getAsProperties() {
		Properties props = new Properties();
		for (String key : configurationValues.keySet()) {
			props.setProperty(key, configurationValues.get(key));
		}
		return props;
	}

	public Map<String, String> getAsMap() {
		return new HashMap<>(this.configurationValues);
	}

	public boolean hasProperty(String key) {
		return this.configurationValues.containsKey(key);
	}

	public Set<String> getPropertyKeys() {
		return new HashSet<>(this.configurationValues.keySet());
	}

	public List<String> getStringList(String key, String defValue) {
		this.defaultValues.put(key, defValue);
		this.valueTypes.put(key, StrolchValueType.STRING_LIST.getType());

		String value = getValue(key, defValue);
		return Arrays.stream(value.split(",")).map(String::trim).filter(s -> !s.isEmpty()).toList();
	}

	public String[] getStringArray(String key, String defValue) {
		return getStringList(key, defValue).toArray(new String[0]);
	}

	public String getString(String key, String defValue) {
		this.defaultValues.put(key, defValue);
		this.valueTypes.put(key, StrolchValueType.STRING.getType());

		return getValue(key, defValue);
	}

	public String getSecret(String key) {
		this.valueTypes.put(key, SECRET);
		return getValue(key, null, true);
	}

	public boolean getBoolean(String key, Boolean defValue) {
		if (defValue != null)
			this.defaultValues.put(key, String.valueOf(defValue));
		this.valueTypes.put(key, StrolchValueType.BOOLEAN.getType());

		String value = this.configurationValues.get(key);
		if (isEmpty(value))
			return handleDefaultNonSecret(key, defValue);

		if (value.equalsIgnoreCase("true"))
			return true;
		if (value.equalsIgnoreCase("false"))
			return false;

		String msg = "Component {0} has non-boolean configuration value for {1} = {2}!";
		msg = MessageFormat.format(msg, this.name, key, value);
		throw new StrolchConfigurationException(msg);
	}

	public int getInt(String key, Integer defValue) {
		if (defValue != null)
			this.defaultValues.put(key, String.valueOf(defValue));
		this.valueTypes.put(key, StrolchValueType.INTEGER.getType());

		String value = this.configurationValues.get(key);
		if (isEmpty(value))
			return handleDefaultNonSecret(key, defValue);

		try {
			return Integer.decode(value);
		} catch (NumberFormatException e) {
			String msg = "Component {0} has non-integer configuration value for {1} = {2}!";
			msg = MessageFormat.format(msg, this.name, key, value);
			throw new StrolchConfigurationException(msg);
		}
	}

	public long getLong(String key, Long defValue) {
		if (defValue != null)
			this.defaultValues.put(key, String.valueOf(defValue));
		this.valueTypes.put(key, StrolchValueType.LONG.getType());

		String value = this.configurationValues.get(key);
		if (isEmpty(value))
			return handleDefaultNonSecret(key, defValue);

		try {
			return Long.parseLong(value);
		} catch (NumberFormatException e) {
			String msg = "Component {0} has non-long configuration value for {1} = {2}!";
			msg = MessageFormat.format(msg, this.name, key, value);
			throw new StrolchConfigurationException(msg);
		}
	}

	public File getConfigFile(String key, String defValue, RuntimeConfiguration configuration) {
		this.defaultValues.put(key, defValue);
		this.valueTypes.put(key, File.class.getSimpleName());

		String value = getValue(key, defValue);

		File configFile = new File(configuration.getConfigPath(), value);
		if (!configFile.isFile() || !configFile.canRead()) {
			String msg
					= "Component {0} requires configuration file for configuration property ''{1}'' which does not exist with value: {2}";
			msg = MessageFormat.format(msg, this.name, key, value);
			throw new StrolchConfigurationException(msg);
		}
		return configFile;
	}

	public File getDataDir(String key, String defValue, RuntimeConfiguration configuration, boolean checkExists) {
		this.defaultValues.put(key, defValue);
		this.valueTypes.put(key, Path.class.getSimpleName());

		String value = getValue(key, defValue);

		File dataDir = new File(configuration.getDataPath(), value);
		if (checkExists && !dataDir.isDirectory() || !dataDir.canRead()) {
			String msg
					= "Component {0} requires data directory for configuration property ''{1}'' which does not exist with value: {2}";
			msg = MessageFormat.format(msg, this.name, key, value);
			throw new StrolchConfigurationException(msg);
		}
		return dataDir;
	}

	public File getDataFile(String key, String defValue, RuntimeConfiguration configuration, boolean checkExists) {
		this.defaultValues.put(key, defValue);
		this.valueTypes.put(key, File.class.getSimpleName());

		String value = getValue(key, defValue);

		File dataFile = new File(configuration.getDataPath(), value);
		if (checkExists && !dataFile.isFile() || !dataFile.canRead()) {
			String msg
					= "Component {0} requires data file for configuration property ''{1}'' which does not exist with value: {2}";
			msg = MessageFormat.format(msg, this.name, key, value);
			throw new StrolchConfigurationException(msg);
		}
		return dataFile;
	}

	private String getValue(String key, String defValue) {
		return getValue(key, defValue, false);
	}

	private String getValue(String key, String defValue, boolean isSecret) {
		String value = this.configurationValues.get(key);
		if (isEmpty(value)) {
			assertDefValueExist(key, defValue);
			logDefValueUse(key, defValue, isSecret);
			value = defValue;
		}
		return value;
	}

	private <T> T handleDefaultNonSecret(String key, T defValue) {
		if (defValue == null)
			throw new IllegalStateException(MessageFormat.format(
					"No configuration value configured for {0} and no default value provided for component {1}", key,
					this.name));

		assertDefValueExist(key, defValue);
		logDefValueUse(key, defValue, false);
		return defValue;
	}

	private void logDefValueUse(String key, Object defValue, boolean isSecret) {
		String msg = "{0}: Using default for key {1}={2}";
		if (isSecret)
			msg = MessageFormat.format(msg, this.name, "***", "***");
		else
			msg = MessageFormat.format(msg, this.name, key, defValue);
		logger.info(msg);
	}

	private void assertDefValueExist(String key, Object defValue) {
		if (defValue == null) {
			String msg = "Component {0} is missing the configuration value with key ''{1}''!";
			msg = MessageFormat.format(msg, this.name, key);
			throw new StrolchConfigurationException(msg);
		}
	}
}
