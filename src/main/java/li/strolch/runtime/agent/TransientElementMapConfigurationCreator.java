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

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.RuntimeConfiguration;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class TransientElementMapConfigurationCreator extends InMemoryElementMapConfigurationCreator {

	@Override
	protected void addConfiguration(List<ComponentConfiguration> configurations,
			RuntimeConfiguration runtimeConfiguration) {

		String name = TransientElementMapController.class.getSimpleName();
		String api = TransientElementMapController.class.getName();
		String impl = TransientElementMapController.class.getName();
		Map<String, String> configurationValues = new HashMap<>();
		Set<String> dependencies = new HashSet<>();
		dependencies.add(ResourceMap.class.getSimpleName());
		dependencies.add(OrderMap.class.getSimpleName());
		ComponentConfiguration configuration = new ComponentConfiguration(runtimeConfiguration, name,
				configurationValues, api, impl, dependencies);
		configurations.add(configuration);
	}
}
