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
package li.strolch.rest.inspector.test;

import java.io.File;
import java.io.IOException;
import java.net.URI;

import javax.ws.rs.core.Application;

import li.strolch.rest.StrolchRestfulExceptionMapper;
import li.strolch.rest.endpoint.Inspector;
import li.strolch.testbase.runtime.RuntimeMock;

import org.glassfish.grizzly.http.server.HttpServer;
import org.glassfish.jersey.filter.LoggingFilter;
import org.glassfish.jersey.grizzly2.servlet.GrizzlyWebContainerFactory;
import org.glassfish.jersey.server.ResourceConfig;
import org.glassfish.jersey.server.ServerProperties;
import org.glassfish.jersey.server.TracingConfig;
import org.glassfish.jersey.test.JerseyTest;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractRestfulTest extends JerseyTest {

	private static final URI BASE_URI = URI.create("http://localhost:8888/base");
	protected static final Logger logger = LoggerFactory.getLogger(AbstractRestfulTest.class);
	private static final String RUNTIME_PATH = "target/withPrivilegeRuntime/"; //$NON-NLS-1$
	private static final String CONFIG_SRC = "src/test/resources/withPrivilegeRuntime"; //$NON-NLS-1$
	private static RuntimeMock runtimeMock;
	private static HttpServer server;

	@BeforeClass
	public static void beforeClass() throws IllegalArgumentException, IOException {

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		runtimeMock.startContainer();

		server = GrizzlyWebContainerFactory.create(BASE_URI);
	}

	@AfterClass
	public static void afterClass() {
		server.shutdownNow();
		runtimeMock.destroyRuntime();
	}

	@Override
	protected Application configure() {
		return createApp();
	}

	public static ResourceConfig createApp() {
		return new ResourceConfig()//
				.packages(Inspector.class.getPackage().getName())//
				.register(StrolchRestfulExceptionMapper.class)
				//.register(createMoxyJsonResolver())
				// Logging.
				.register(LoggingFilter.class)
				// Tracing support.
				.property(ServerProperties.TRACING, TracingConfig.ON_DEMAND.name());
	}
}
