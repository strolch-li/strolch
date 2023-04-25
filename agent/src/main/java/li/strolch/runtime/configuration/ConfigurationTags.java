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

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ConfigurationTags {

	public static final String ENV_GLOBAL = "global";

	public static final String STROLCH_CONFIGURATION_ENV_COMPONENT_PROPERTIES = "StrolchConfiguration/env/Component/Properties";
	public static final String STROLCH_CONFIGURATION_ENV_COMPONENT = "StrolchConfiguration/env/Component";
	public static final String STROLCH_CONFIGURATION_ENV_RUNTIME_PROPERTIES = "StrolchConfiguration/env/Runtime/Properties";
	public static final String STROLCH_CONFIGURATION_ENV_RUNTIME = "StrolchConfiguration/env/Runtime";
	public static final String STROLCH_CONFIGURATION_ENV = "StrolchConfiguration/env";

	public static final String APPLICATION_NAME = "applicationName";
	public static final String ID = "id";
	public static final String DEPENDS = "depends";
	public static final String IMPL = "impl";
	public static final String API = "api";
	public static final String NAME = "name";

}
