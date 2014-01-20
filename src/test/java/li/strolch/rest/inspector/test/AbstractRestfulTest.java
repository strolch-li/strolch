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

import li.strolch.rest.inspector.AgentRef;
import li.strolch.rest.inspector.StrolchRestfulClasses;
import li.strolch.rest.inspector.StrolchRestfulExceptionMapper;
import li.strolch.service.api.ServiceHandler;
import li.strolch.testbase.runtime.RuntimeMock;

import org.glassfish.grizzly.http.server.HttpServer;
import org.glassfish.jersey.grizzly2.httpserver.GrizzlyHttpServerFactory;
import org.glassfish.jersey.server.ResourceConfig;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractRestfulTest {

	protected static final String BASE_URI = "http://localhost:8080/tutorialwebapp";
	protected static final Logger logger = LoggerFactory.getLogger(AbstractRestfulTest.class);

	private static final String RUNTIME_PATH = "target/withPrivilegeRuntime/"; //$NON-NLS-1$
	private static final String CONFIG_SRC = "src/test/resources/withPrivilegeRuntime"; //$NON-NLS-1$
	private static RuntimeMock runtimeMock;
	private static HttpServer httpServer;

	@BeforeClass
	public static void beforeClass() {

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		runtimeMock.startContainer(rootPath);

		AgentRef.getInstance().init(runtimeMock.getAgent());

		// create a resource config that scans for JAX-RS resources and providers
		// in com.example package
		final ResourceConfig rc = new ResourceConfig();
		for (Class<?> clazz : StrolchRestfulClasses.getRestfulClasses()) {
			rc.register(clazz);
			rc.register(StrolchRestfulExceptionMapper.class);
		}

		httpServer = GrizzlyHttpServerFactory.createHttpServer(URI.create(BASE_URI), rc);
	}

	@AfterClass
	public static void afterClass() {
		httpServer.shutdownNow();
		runtimeMock.destroyRuntime();
	}

	public static ServiceHandler getServiceHandler() {
		return runtimeMock.getContainer().getComponent(ServiceHandler.class);
	}
}
