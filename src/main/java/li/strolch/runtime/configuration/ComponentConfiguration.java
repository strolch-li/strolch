package li.strolch.runtime.configuration;

import java.util.Map;
import java.util.Set;

public class ComponentConfiguration extends AbstractionConfiguration {

	private final RuntimeConfiguration runtimeConfiguration;

	private final String api;
	private final String impl;
	private final Set<String> dependencies;

	public ComponentConfiguration(RuntimeConfiguration runtimeConfiguration, String name,
			Map<String, String> configurationValues, String api, String impl, Set<String> dependencies) {
		super(name, configurationValues);
		this.runtimeConfiguration = runtimeConfiguration;
		this.api = api;
		this.impl = impl;
		this.dependencies = dependencies;
	}

	public RuntimeConfiguration getRuntimeConfiguration() {
		return this.runtimeConfiguration;
	}

	public String getApi() {
		return this.api;
	}

	public String getImpl() {
		return this.impl;
	}

	public Set<String> getDependencies() {
		return this.dependencies;
	}
}
