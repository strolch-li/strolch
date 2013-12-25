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
package li.strolch.runtime.agent.impl;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import li.strolch.runtime.agent.api.ElementMapHandler;
import li.strolch.runtime.agent.api.StrolchAgent;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.RuntimeConfiguration;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class TransientElementMapHandlerConfigurator implements ElementMapHandlerConfigurator {

	@Override
	public ComponentConfiguration buildConfiguration(StrolchAgent agent) {

		String name = ElementMapHandler.class.getSimpleName();
		String api = ElementMapHandler.class.getName();
		String impl = TransientElementMapHandler.class.getName();

		Map<String, String> configurationValues = new HashMap<>();
		Set<String> dependencies = Collections.emptySet();

		RuntimeConfiguration runtimeConfiguration = agent.getStrolchConfiguration().getRuntimeConfiguration();
		ComponentConfiguration configuration = new ComponentConfiguration(runtimeConfiguration, name,
				configurationValues, api, impl, dependencies);

		return configuration;
	}
}
