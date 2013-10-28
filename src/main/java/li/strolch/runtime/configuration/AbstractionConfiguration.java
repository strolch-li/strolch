package li.strolch.runtime.configuration;

import java.io.File;
import java.text.MessageFormat;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.StringHelper;

public class AbstractionConfiguration {

	private static final Logger logger = LoggerFactory.getLogger(AbstractionConfiguration.class);

	private final String componentName;
	private final Map<String, String> configurationValues;

	public AbstractionConfiguration(String componentName, Map<String, String> configurationValues) {
		this.componentName = componentName;
		this.configurationValues = configurationValues;
	}

	public String getComponentName() {
		return this.componentName;
	}

	public String getString(String key, String defValue) {
		String value = this.configurationValues.get(key);
		if (!StringHelper.isEmpty(value)) {
			return value;
		}

		assertDefValueExist(key, defValue);
		logDefValueUse(key, defValue);
		return defValue;
	}

	public boolean getBoolean(String key, Boolean defValue) {
		String value = this.configurationValues.get(key);
		if (!StringHelper.isEmpty(value)) {
			if (value.equalsIgnoreCase("true")) //$NON-NLS-1$
				return true;
			else if (value.equalsIgnoreCase("false")) //$NON-NLS-1$
				return false;

			String msg = "Component {0} has non-boolean configuration value for {1} = {2}!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, this.componentName, key, value);
			throw new StrolchConfigurationException(msg);
		}

		assertDefValueExist(key, defValue);
		logDefValueUse(key, defValue);
		return defValue;
	}

	public int getInt(String key, int defValue) {
		String value = this.configurationValues.get(key);
		if (!StringHelper.isEmpty(value)) {

			try {
				return Integer.parseInt(value);
			} catch (NumberFormatException e) {
				String msg = "Component {0} has non-integer configuration value for {1} = {2}!"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, this.componentName, key, value);
				throw new StrolchConfigurationException(msg);
			}
		}

		assertDefValueExist(key, defValue);
		logDefValueUse(key, defValue);
		return defValue;
	}

	public long getLong(String key, long defValue) {
		String value = this.configurationValues.get(key);
		if (!StringHelper.isEmpty(value)) {

			try {
				return Long.parseLong(value);
			} catch (NumberFormatException e) {
				String msg = "Component {0} has non-long configuration value for {1} = {2}!"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, this.componentName, key, value);
				throw new StrolchConfigurationException(msg);
			}
		}

		assertDefValueExist(key, defValue);
		logDefValueUse(key, defValue);
		return defValue;
	}

	public File getConfigFile(String key, String defValue, ComponentConfiguration configuration) {
		String value = this.configurationValues.get(key);
		if (StringHelper.isEmpty(value)) {
			assertDefValueExist(key, defValue);
			logDefValueUse(key, defValue);
			value = defValue;
		}

		File configFile = new File(configuration.getRuntimeConfiguration().getConfigPath(), value);
		if (!configFile.isFile() || !configFile.canRead()) {

			String msg = "Component {0} is missing required configuration file {1}!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, this.componentName, key, value);
			throw new StrolchConfigurationException(msg);
		}
		return configFile;
	}

	private void logDefValueUse(String key, Object defValue) {
		String msg = "Using default for key {0}={1}"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, key, defValue);
		logger.info(msg);
	}

	private void assertDefValueExist(String key, Object defValue) {
		if (defValue == null) {
			String msg = "Component {0} is missing the configuration value for key {1} does not exist!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, this.componentName, key);
			throw new StrolchConfigurationException(msg);
		}
	}
}
