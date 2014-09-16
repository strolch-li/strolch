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
