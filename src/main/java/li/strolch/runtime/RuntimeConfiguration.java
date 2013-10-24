package li.strolch.runtime;

import java.util.Map;

public class RuntimeConfiguration extends AbstractionConfiguration {

	private static final String RUNTIME = "Runtime"; //$NON-NLS-1$
	private final String rootPath;

	public RuntimeConfiguration(Map<String, String> configurationValues, String rootPath) {
		super(RUNTIME, configurationValues);
		this.rootPath = rootPath;
	}

	public String getRootPath() {
		return this.rootPath;
	}
}
