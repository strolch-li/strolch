package li.strolch.runtime.configuration;

import java.io.File;
import java.text.MessageFormat;
import java.util.Map;

public class RuntimeConfiguration extends AbstractionConfiguration {

	public static final String RUNTIME = "Runtime"; //$NON-NLS-1$
	public static final String PATH_CONFIG = "config"; //$NON-NLS-1$
	public static final String PATH_DATA = "data"; //$NON-NLS-1$

	private final String applicationName;
	private final File rootPath;
	private final File configPath;
	private final File dataPath;

	public RuntimeConfiguration(String applicationName, Map<String, String> configurationValues, File rootPathF) {
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

		File dataPathF = new File(rootPathF, PATH_CONFIG);
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
		this.rootPath = rootPathF;
		this.configPath = configPathF;
		this.dataPath = dataPathF;
	}

	public String getApplicationName() {
		return this.applicationName;
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
}
