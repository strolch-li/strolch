package li.strolch.runtime;

import java.util.Map;

public class RuntimeConfiguration extends ComponentConfiguration {

	private final String rootPath;

	public RuntimeConfiguration(String componentName, Map<String, String> configurationValues, String rootPath) {
		super(componentName, configurationValues);
		this.rootPath = rootPath;
	}

	public String getRootPath() {
		return this.rootPath;
	}
}
