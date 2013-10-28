package li.strolch.runtime.configuration;

import java.io.File;
import java.text.MessageFormat;
import java.util.Map;

public class RuntimeConfiguration extends AbstractionConfiguration {

	private static final String RUNTIME = "Runtime"; //$NON-NLS-1$
	private static final String PATH_CONFIG = "config"; //$NON-NLS-1$

	private final File rootPath;
	private final File configPath;

	public RuntimeConfiguration(Map<String, String> configurationValues, String rootPath) {
		super(RUNTIME, configurationValues);
		File rootPathF = new File(rootPath);
		if (!rootPathF.isDirectory() || !rootPathF.canRead()) {
			String msg = "Root path is not readable at {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, rootPath);
			throw new StrolchConfigurationException(msg);
		}

		File configPathF = new File(rootPath, PATH_CONFIG);
		if (!configPathF.isDirectory() || !configPathF.canRead()) {
			String msg = "Config path is not readable at {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, configPathF);
			throw new StrolchConfigurationException(msg);
		}

		this.rootPath = rootPathF;
		this.configPath = configPathF;
	}

	public File getRootPath() {
		return this.rootPath;
	}

	public File getConfigPath() {
		return this.configPath;
	}
}
