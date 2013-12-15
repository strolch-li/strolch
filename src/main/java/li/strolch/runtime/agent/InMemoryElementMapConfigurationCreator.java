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
package li.strolch.runtime.agent;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.RuntimeConfiguration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public abstract class InMemoryElementMapConfigurationCreator implements ElementMapConfigurationCreator {

	protected static final Logger logger = LoggerFactory.getLogger(InMemoryElementMapConfigurationCreator.class);

	@Override
	public List<ComponentConfiguration> getComponentConfigurations(RuntimeConfiguration runtimeConfiguration) {
		List<ComponentConfiguration> configurations = new ArrayList<>();
		addConfiguration(configurations, runtimeConfiguration);
		addResourceMapConfiguration(configurations, runtimeConfiguration);
		addOrderMapConfiguration(configurations, runtimeConfiguration);
		return configurations;
	}

	protected abstract void addConfiguration(List<ComponentConfiguration> configurations,
			RuntimeConfiguration runtimeConfiguration);

	protected void addResourceMapConfiguration(List<ComponentConfiguration> configurations,
			RuntimeConfiguration runtimeConfiguration) {

		String name = ResourceMap.class.getSimpleName();
		String api = ResourceMap.class.getName();
		String impl = InMemoryResourceMap.class.getName();
		Map<String, String> configurationValues = new HashMap<>();
		Set<String> dependencies = Collections.emptySet();
		ComponentConfiguration configuration = new ComponentConfiguration(runtimeConfiguration, name,
				configurationValues, api, impl, dependencies);
		configurations.add(configuration);
	}

	protected void addOrderMapConfiguration(List<ComponentConfiguration> configurations,
			RuntimeConfiguration runtimeConfiguration) {

		String name = OrderMap.class.getSimpleName();
		String api = OrderMap.class.getName();
		String impl = InMemoryOrderMap.class.getName();
		Map<String, String> configurationValues = new HashMap<>();
		Set<String> dependencies = Collections.emptySet();

		ComponentConfiguration configuration = new ComponentConfiguration(runtimeConfiguration, name,
				configurationValues, api, impl, dependencies);
		configurations.add(configuration);
	}
}
