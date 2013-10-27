package li.strolch.runtime.configuration;

import java.util.Map;

public class ComponentConfiguration extends AbstractionConfiguration {

	private final RuntimeConfiguration runtimeConfiguration;

	public ComponentConfiguration(RuntimeConfiguration runtimeConfiguration, String componentName,
			Map<String, String> configurationValues) {
		super(componentName, configurationValues);
		this.runtimeConfiguration = runtimeConfiguration;
	}

	public RuntimeConfiguration getRuntimeConfiguration() {
		return this.runtimeConfiguration;
	}
}
