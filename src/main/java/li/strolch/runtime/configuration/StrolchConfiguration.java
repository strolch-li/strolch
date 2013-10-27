package li.strolch.runtime.configuration;

import java.text.MessageFormat;
import java.util.Map;

public class StrolchConfiguration {

	private final RuntimeConfiguration runtimeConfiguration;
	private final Map<String, AbstractionConfiguration> configurationByComponent;

	public StrolchConfiguration(RuntimeConfiguration runtimeConfiguration,
			Map<String, AbstractionConfiguration> configurationByComponent) {
		this.runtimeConfiguration = runtimeConfiguration;
		this.configurationByComponent = configurationByComponent;
	}

	public RuntimeConfiguration getRuntimeConfiguration() {
		return this.runtimeConfiguration;
	}

	public AbstractionConfiguration getComponentConfiguration(String componentName) {
		AbstractionConfiguration componentConfiguration = this.configurationByComponent.get(componentName);
		if (componentConfiguration == null) {
			String msg = "No configuration exists for the component {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, componentName);
			throw new StrolchConfigurationException(msg);
		}
		return componentConfiguration;
	}
}
