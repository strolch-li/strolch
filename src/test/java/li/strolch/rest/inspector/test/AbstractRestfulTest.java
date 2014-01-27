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

import javax.ws.rs.core.MediaType;

import li.strolch.rest.StrolchRestfulClasses;
import li.strolch.rest.StrolchRestfulExceptionMapper;
import li.strolch.testbase.runtime.RuntimeMock;

import org.eclipse.persistence.jaxb.rs.MOXyJsonProvider;
import org.glassfish.grizzly.http.server.HttpServer;
import org.glassfish.jersey.grizzly2.httpserver.GrizzlyHttpServerFactory;
import org.glassfish.jersey.server.ResourceConfig;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.client.config.ClientConfig;
import com.sun.jersey.api.client.config.DefaultClientConfig;
import com.sun.jersey.api.representation.Form;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractRestfulTest {

	protected static final String BASE_URI = "http://localhost:56789/tutorialwebapp";
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

	protected WebResource getResource() {
		ClientConfig cc = new DefaultClientConfig();
		cc.getClasses().add(MOXyJsonProvider.class);
		Client client = Client.create(cc);
		WebResource resource = client.resource(BASE_URI);
		return resource;
	}

	protected ClientResponse doGet(String path) {
		WebResource resource = getResource();
		ClientResponse response = resource.path(path).accept(MediaType.APPLICATION_JSON_TYPE).get(ClientResponse.class);
		if (response.getStatus() != ClientResponse.Status.OK.getStatusCode())
			throw new RuntimeException("Failed to get from path " + path + " due to "
					+ response.getEntity(String.class));
		return response;
	}

	protected <T> ClientResponse doPostForm(String path, Form form) {
		WebResource resource = getResource();
		ClientResponse response = resource.path(path).type(MediaType.APPLICATION_FORM_URLENCODED)
				.accept(MediaType.APPLICATION_JSON_TYPE).post(ClientResponse.class, form);
		if (response.getStatus() != ClientResponse.Status.OK.getStatusCode())
			throw new RuntimeException("Failed to post to path " + path + " due to " + response.getEntity(String.class));
		return response;
	}
}
