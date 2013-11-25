/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
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
