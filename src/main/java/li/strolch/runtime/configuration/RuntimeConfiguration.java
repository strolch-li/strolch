package li.strolch.runtime.configuration;

import java.io.File;
import java.text.MessageFormat;
import java.util.Map;

public class RuntimeConfiguration extends AbstractionConfiguration {

	private static final String RUNTIME = "Runtime"; //$NON-NLS-1$
	private final String rootPath;

	public RuntimeConfiguration(Map<String, String> configurationValues, String rootPath) {
		super(RUNTIME, configurationValues);
		File rootPathF = new File(rootPath);
		if (!rootPathF.isDirectory() || !rootPathF.canRead()) {
			String msg = "Root path is not readable at {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, rootPath);
			throw new StrolchConfigurationException(msg);
		}
		this.rootPath = rootPath;
	}

	public String getRootPath() {
		return this.rootPath;
	}
}
