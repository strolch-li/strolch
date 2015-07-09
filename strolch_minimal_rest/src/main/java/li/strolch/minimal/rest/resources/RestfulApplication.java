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
package li.strolch.minimal.rest.resources;

import javax.ws.rs.ApplicationPath;

import org.glassfish.jersey.filter.LoggingFilter;
import org.glassfish.jersey.server.ResourceConfig;
import org.glassfish.jersey.server.ServerProperties;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@ApplicationPath(value = "rest")
public class RestfulApplication extends ResourceConfig {

	public RestfulApplication() {

		packages(GreetingsResource.class.getPackage().getName());

		//register(CharsetResponseFilter.class);
		//register(HttpCacheResponseFilter.class);
		//register(RestfulExceptionMapper.class);

		register(new LoggingFilter(java.util.logging.Logger.getGlobal(), true));

		property(ServerProperties.TRACING, "ALL");
		property(ServerProperties.TRACING_THRESHOLD, "TRACE");
	}
}
