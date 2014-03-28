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
import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import javax.ws.rs.core.Application;
import javax.ws.rs.ext.ContextResolver;

import li.strolch.testbase.runtime.RuntimeMock;

import org.glassfish.grizzly.http.server.HttpServer;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.grizzly2.httpserver.GrizzlyHttpServerFactory;
import org.glassfish.jersey.moxy.json.MoxyJsonConfig;
import org.glassfish.jersey.server.ResourceConfig;
import org.glassfish.jersey.test.JerseyTest;
import org.glassfish.jersey.test.TestProperties;
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
	public static void beforeClass() {

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		runtimeMock.startContainer();

		server = GrizzlyHttpServerFactory.createHttpServer(BASE_URI, createApp());
	}

	@AfterClass
	public static void afterClass() {
		server.shutdownNow();
		runtimeMock.destroyRuntime();
	}

	@Override
	protected Application configure() {
		enable(TestProperties.LOG_TRAFFIC);
		enable(TestProperties.DUMP_ENTITY);

		return createApp();
	}

	@Override
	protected void configureClient(ClientConfig config) {
		config.register(createMoxyJsonResolver());
	}

	public static ResourceConfig createApp() {
		return new ResourceConfig().packages("li.strolch.rest.endpoint").register(createMoxyJsonResolver());
	}

	public static ContextResolver<MoxyJsonConfig> createMoxyJsonResolver() {
		final MoxyJsonConfig moxyJsonConfig = new MoxyJsonConfig();
		Map<String, String> namespacePrefixMapper = new HashMap<String, String>(1);
		namespacePrefixMapper.put("http://www.w3.org/2001/XMLSchema-instance", "xsi");
		moxyJsonConfig.setNamespacePrefixMapper(namespacePrefixMapper).setNamespaceSeparator(':');
		return moxyJsonConfig.resolver();
	}
}
